open Lambda

exception ParcError

module Logging = struct
  let log ppf tag expr =
    Format.fprintf ppf "[%s]@.%a@.@." tag Printlambda.lambda expr
end
module Vset = Ident.Set

type env = { borrowed : Vset.t; owned : Vset.t }

let empty_env = { borrowed = Vset.empty; owned = Vset.empty }

let ppf = Format.std_formatter

let parc expr =
  let rec parc_regular ({ borrowed; owned } as env) expr =
    match expr with
    | Lvar x ->
        (* Rule SVar and SVar-Dup *)
        Logging.log ppf "parc_helper: Lvar" expr ;
        if Vset.is_empty owned && Vset.mem x borrowed then
          Lsequence (Refcnt.dup expr, expr)
        else if Vset.equal owned (Vset.singleton x) && not (Vset.mem x borrowed)
        then
          expr
        else
          raise ParcError
    | Lconst _ ->
        Logging.log ppf "parc_helper: Lconst" expr;
        expr
    | Lapply ({ ap_func; ap_args } as app) ->
        (* Rule SApp

           Extended to handle multi-argument functions. Assumes left-to-right
           evaluation order.

           TODO(1ntEgr8): Do we have to do anything special to ensure
           left-to-right evaluation order is a guaranteed?
        *)
        Logging.log ppf "parc_helper: Lapply" expr;
        let env', ap_args' = parc_many env ap_args in
        let ap_func' = parc_regular env' ap_func in
        Lapply { app with ap_func = ap_func'; ap_args = ap_args' }
    | Lfunction { kind; params; return; body; attr; loc } ->
        Logging.log ppf "parc_helper: Lfunction" expr;
        (* Rule SLam and SLam-D *)
        let xs = Vset.of_list (List.map fst params) in
        let ys = free_variables expr in
        let body_fvs = free_variables body in
        let should_own, should_drop =
          Vset.partition (fun x -> Vset.mem x body_fvs) xs
        in
        let should_dup = Vset.diff ys owned in
        let body' =
          let body'' =
            parc_regular
              { borrowed = Vset.empty; owned = Vset.union ys should_own }
              body
          in
          (* Insert drops *)
          Vset.fold
            (fun x acc -> Lsequence (Refcnt.drop (Lvar x), acc))
            should_drop body''
        in
        let func' = lfunction ~kind ~params ~return ~body:body' ~attr ~loc in
        (* Insert dups *)
        Vset.fold
          (fun x acc -> Lsequence (Refcnt.dup (Lvar x), acc))
          should_dup func'
    | Llet (_let_kind, value_kind, x, e1, e2) ->
        (* Rule SBind and SBind-D *)
        Logging.log ppf "parc_helper: Llet" expr;
        if Vset.mem x borrowed || Vset.mem x owned then raise ParcError
        else
          let e2_fvs = free_variables e2 in
          let owned2 = Vset.inter owned (Vset.remove x e2_fvs) in
          let owned' =
            if Vset.mem x e2_fvs then Vset.add x owned2 else owned2
          in
          let e1' =
            parc_regular
              {
                borrowed = Vset.union borrowed owned2;
                owned = Vset.diff owned owned2;
              }
              e1
          in
          let e2' = parc_regular { borrowed; owned = owned' } e2 in

          (* TODO(1ntEgr8):
             Optimization - Set let_kind to Alias if transformed
             expression is pure
          *)
          let let_kind' = Strict in

          Llet (let_kind', value_kind, x, e1', e2')
    | Lletrec ([(x, e1)] , e2) ->
        (* Rule SBind and SBind-D *)
        Logging.log ppf "parc_helper: Lletrec([(x, e1)], e2)" expr;
        if Vset.mem x borrowed || Vset.mem x owned then raise ParcError
        else
          let e2_fvs = free_variables e2 in
          let owned2 = Vset.inter owned (Vset.remove x e2_fvs) in
          let owned' =
            if Vset.mem x e2_fvs then Vset.add x owned2 else owned2
          in
          let e1' =
            parc_regular
              {
                borrowed = Vset.union borrowed owned2;
                owned = Vset.diff owned owned2;
              }
              e1
          in
          let e2' = parc_regular { borrowed; owned = owned' } e2 in
          Lletrec ([(x, e1')], e2')
    | Lprim ((Pmakeblock _ as p), args, loc) ->
      Logging.log ppf "parc_helper: Lprim(makeblock)" expr;
      Lprim (p, snd (parc_many env args), loc)
    | Lsequence (e1, e2) ->
        let _, exprs = parc_many env [e1 ; e2] in
        Lsequence (List.hd exprs, List.hd (List.tl exprs))
    | Lmarker (Match_begin, lam) ->
      Logging.log ppf "parc_helper: Lmarker(Match_begin)" expr;
        parc_borrowed env lam
    | Lmarker (Matched_body _, _) ->
      Logging.log ppf "parc_helper: Lmarker(Matched_body)" expr;
        raise ParcError
    | _ -> 
        Logging.log ppf "parc_helper: unhandled" expr;
        expr
  and parc_borrowed env expr =
    match expr with
    | Lmarker (Match_begin, _) -> raise ParcError
    | Lmarker (Matched_body _, lam) ->
        (* TODO change the environment as per bindings *)
        let env' = env in
        parc_regular env' lam
    | _ -> shallow_map (fun e -> parc_borrowed env e) expr
  and parc_many env exprs =
    List.fold_left_map
      (fun ({ borrowed; owned } as env) e ->
        let owned' = Vset.inter owned (free_variables e) in
        let e' = parc_regular { env with owned = owned' } e in
        let env' =
          {
            borrowed = Vset.union borrowed owned';
            owned = Vset.diff owned owned';
          }
        in
        (env', e'))
      env exprs
  in
  Logging.log ppf "parc: begin" expr;
  parc_regular empty_env expr

let parc_program program =
  let code = parc program.code in
  { program with code }
