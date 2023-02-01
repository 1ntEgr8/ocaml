open Lambda

exception ParcError

module Vset = Ident.Set

type env = { borrowed : Vset.t; owned : Vset.t }

let empty_env = { borrowed = Vset.empty; owned = Vset.empty }

let parc expr =
  let rec parc_regular ({ borrowed; owned } as env) expr =
    match expr with
    | Lvar x ->
        if Vset.is_empty owned && Vset.mem x borrowed then
          (* Rule SVar-Dup *)
          Lsequence (Refcnt.dup expr, expr)
        else if Vset.equal owned (Vset.singleton x) && not (Vset.mem x borrowed)
        then (* Rule SVar *)
          expr
        else (* ERROR *)
          raise ParcError
    | Lconst _ -> expr
    | Lapply ({ ap_func; ap_args } as app) ->
        (* Rule SApp

           Extended to handle multi-argument functions. Assumes left-to-right
           evaluation order.

           TODO(1ntEgr8): Do we have to do anything special to ensure
           left-to-right evaluation order is a guaranteed?
        *)
        let env', ap_args' =
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
            env ap_args
        in
        let ap_func' = parc_regular env' ap_func in
        Lapply { app with ap_func = ap_func'; ap_args = ap_args' }
    | Lfunction { kind; params; return; body; attr; loc } ->
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
          if Vset.is_empty should_drop then body''
          else
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
    | Lmarker (Match_begin, lam) -> parc_borrowed env lam
    | _ -> expr
  and parc_borrowed env expr =
    match expr with
    | Lmarker (Match_begin, _) -> raise ParcError
    | Lmarker (Matched_body, lam) ->
        (* TODO change the environment as per bindings *)
        let env' = env in
        parc_regular env' lam
    | _ -> shallow_map (fun e -> parc_borrowed env e) expr
  in
  parc_regular empty_env expr
