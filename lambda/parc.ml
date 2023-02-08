open Lambda
open Refcnt

exception ParcError

module Vset = Ident.Set

type env = {
  borrowed : Vset.t;
  owned : Vset.t;
  matched_expression : Ident.t option;
  bound_funcs : Vset.t;
}

module Logging = struct
  let log ppf tag { borrowed; owned; bound_funcs } expr =
    if !Clflags.dump_parc then
      Format.fprintf ppf "[%s]@.borrowed=%a@.owned=%a@.bound_funcs=%a@.%a@.@."
        tag Vset.print borrowed Vset.print owned Vset.print bound_funcs
        Printlambda.lambda expr

  let dump_if ppf expr =
    if !Clflags.dump_parc then (
      Format.fprintf ppf "@.automated_refcounting:@.";
      Printlambda.lambda ppf expr ;
      Format.pp_print_newline ppf ();
    );
    expr
end

let empty_env =
  {
    borrowed = Vset.empty;
    owned = Vset.empty;
    matched_expression = None;
    bound_funcs = Vset.empty;
  }

let ppf = Format.std_formatter

let free_variables { bound_funcs } expr =
  Vset.diff (Lambda.free_variables expr) bound_funcs

let parc expr =
  let rec parc_regular ({ borrowed; owned; bound_funcs } as env) expr =
    match expr with
    | Lvar x ->
        (* Rule SVar and SVar-Dup *)
        Logging.log ppf "parc_helper: Lvar" env expr;
        if Vset.mem x bound_funcs then expr
        else if Vset.is_empty owned && Vset.mem x borrowed then
          with_dup x expr
        else if Vset.equal owned (Vset.singleton x) && not (Vset.mem x borrowed)
        then expr
        else raise ParcError
    | Lconst _ ->
        Logging.log ppf "parc_helper: Lconst" env expr;
        expr
    | Lapply ({ ap_func; ap_args } as app) ->
        (* Rule SApp

           Extended to handle multi-argument functions. Assumes left-to-right
           evaluation order.

           TODO(1ntEgr8): Do we have to do anything special to ensure
           left-to-right evaluation order is guaranteed?
        *)
        Logging.log ppf "parc_helper: Lapply" env expr;
        let env', ap_args' = parc_many env ap_args in
        let ap_func' = parc_regular env' ap_func in
        Lapply { app with ap_func = ap_func'; ap_args = ap_args' }
    | Lfunction { kind; params; return; body; attr; loc } ->
        Logging.log ppf "parc_helper: Lfunction" env expr;
        (* Rule SLam and SLam-D *)
        let xs = Vset.of_list (List.map fst params) in
        let ys = free_variables env expr in
        let body_fvs = free_variables env body in
        let should_own, should_drop =
          Vset.partition (fun x -> Vset.mem x body_fvs) xs
        in
        let should_dup = Vset.diff ys owned in
        let body' =
          let body'' =
            parc_regular
              {
                env with
                borrowed = Vset.empty;
                owned = Vset.union ys should_own;
              }
              body
          in
          (* Insert drops *)
          with_drops should_drop body''
        in
        let func' = lfunction ~kind ~params ~return ~body:body' ~attr ~loc in
        (* Insert dups *)
        with_dups should_dup func'
    | Llet (_let_kind, value_kind, x, e1, e2) ->
        (* Rule SBind and SBind-D *)
        Logging.log ppf "parc_helper: Llet" env expr;
        if Vset.mem x borrowed || Vset.mem x owned then raise ParcError
        else
          let e2_fvs = free_variables env e2 in
          let owned2 = Vset.inter owned (Vset.remove x e2_fvs) in
          let owned' =
            if Vset.mem x e2_fvs then Vset.add x owned2 else owned2
          in
          let e1' =
            parc_regular
              {
                env with
                borrowed = Vset.union borrowed owned2;
                owned = Vset.diff owned owned2;
              }
              e1
          in
          let e2' = parc_regular { env with borrowed; owned = owned' } e2 in

          (* TODO(1ntEgr8):
             Optimization - Set let_kind to Alias if transformed
             expression is pure
          *)
          let let_kind' = Strict in

          Llet (let_kind', value_kind, x, e1', e2')
    | Lletrec ([ (x, e1) ], e2) ->
        (* TODO two cases here
           - function defs
            - here, we do not want to dup/drop binders
           - recursive definitions of values
            - here, we must dup/drop binders
        *)

        (* Rule SBind and SBind-D *)
        Logging.log ppf "parc_helper: Lletrec([(x, e1)], e2)" env expr;
        if Vset.mem x borrowed || Vset.mem x owned then raise ParcError
        else
          let e2_fvs = free_variables env e2 in
          let owned2 = Vset.inter owned (Vset.remove x e2_fvs) in
          let owned' =
            if Vset.mem x e2_fvs then Vset.add x owned2 else owned2
          in
          let e1' =
            parc_regular
              {
                env with
                borrowed = Vset.union borrowed owned2;
                owned = Vset.diff owned owned2;
                bound_funcs = Vset.add x bound_funcs;
              }
              e1
          in
          let e2' =
            parc_regular
              {
                env with
                borrowed;
                owned = owned';
                bound_funcs = Vset.add x bound_funcs;
              }
              e2
          in
          Lletrec ([ (x, e1') ], e2')
    | Lprim ((Pmakeblock _ as p), args, loc) ->
        Logging.log ppf "parc_helper: Lprim(makeblock)" env expr;
        Lprim (p, snd (parc_many env args), loc)
    | Lprim ((Pccall desc as p), args, loc)
      when Primitive.native_name desc = dup_copy_native_name ->
        Logging.log ppf "parc_helper: Lprim(dup_copy)" env expr;
        assert (List.length args == 1);
        let _, args' = parc_many env args in
        Lprim (p, args', loc)
    | Lifthenelse (cond, e1, e2) ->
        let owned_e1 = Vset.inter owned (free_variables env e1) in
        let should_drop_e1 = Vset.diff owned owned_e1 in
        let owned_e2 = Vset.inter owned (free_variables env e2) in
        let should_drop_e2 = Vset.diff owned owned_e2 in
        let e1' =
          parc_regular { env with owned= owned_e1 } e1
          |> with_drops should_drop_e1
        in
        let e2' =
          parc_regular { env with owned= owned_e2 } e2
          |> with_drops should_drop_e2
        in
        let cond' = 
          parc_regular
          {
            env with
            borrowed= Vset.union (Vset.union borrowed owned_e1) owned_e2 ;
            owned= Vset.diff (Vset.diff owned owned_e1) owned_e2 ;
          }
          cond
        in
        Lifthenelse (cond', with_drops should_drop_e1 e1', e2')
    | Lsequence (e1, e2) ->
        let _, exprs = parc_many env [ e1; e2 ] in
        Lsequence (List.hd exprs, List.hd (List.tl exprs))
    | Lmarker (Match_begin id, lam) ->
        Logging.log ppf "parc_helper: Lmarker(Match_begin)" env expr;
        let env' =
          {
            env with
            owned = Vset.remove id owned;
            matched_expression = Some id;
          }
        in
        let lam' = parc_borrowed env' lam in
        Lmarker (Match_begin id, lam')
    | Lmarker (Matched_body _, _) ->
        Logging.log ppf "parc_helper: Lmarker(Matched_body)" env expr;
        raise ParcError
    | _ ->
        Logging.log ppf "parc_helper: unhandled" env expr;
        expr
  and parc_borrowed ({ owned; matched_expression } as env) expr =
    match expr with
    | Lmarker (Match_begin _, _) ->
        Logging.log ppf "parc_borrowed: Lmarker(Match_begin)" env expr;
        raise ParcError
    | Lmarker (Matched_body pat, lam) when Option.is_some matched_expression ->
        Logging.log ppf "parc_borrowed: Lmarker(Matched_body)" env expr;
        let id = Option.get matched_expression in
        let bv = Vset.of_list (Typedtree.pat_bound_idents pat) in
        let fv = free_variables env lam in
        let owned_bv = Vset.union owned bv in
        let owned' = Vset.inter owned_bv fv in
        let should_drop = Vset.diff owned_bv owned' in
        let e' = parc_regular { env with owned = owned' } lam in
        let lam' =
          with_drops should_drop e'
          (* Prepend additional dups and drops pertaining to bound pattern
              variables and the matched variable

             Specifically,

             dup <bound variables> ;
             drop <matched variable>
          *)
          |> with_drop id
          |> with_dups bv
        in
        Lmarker (Matched_body pat, lam')
    | Lmarker (Matched_body _, _) when Option.is_none matched_expression ->
        raise ParcError
    | _ ->
        Logging.log ppf "parc_borrowed: catch all" env expr;
        shallow_map (fun e -> parc_borrowed env e) expr
  and parc_many env exprs =
    List.fold_right
      (fun e (({ borrowed; owned } as env), es) ->
        let owned' = Vset.inter owned (free_variables env e) in
        let e' = parc_regular { env with owned = owned' } e in
        let env' =
          {
            env with
            borrowed = Vset.union borrowed owned';
            owned = Vset.diff owned owned';
          }
        in
        (env', e' :: es))
      exprs (env, [])
  in
  Logging.log ppf "parc: begin" empty_env expr;
  parc_regular empty_env expr |> Logging.dump_if ppf

let parc_program program =
  let code = parc program.code in
  { program with code }
