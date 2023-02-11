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

module Mode = struct
  type mode = Regular | Borrowed

  let is_borrowed = function Borrowed -> true | _ -> false
  let is_regular = function Regular -> true | _ -> false

  let print ppf m =
    let s = match m with Regular -> "regular" | Borrowed -> "borrowed" in
    Format.fprintf ppf "%s" s
end

module Logging = struct
  let log ppf tag mode { borrowed; owned; bound_funcs } expr =
    if !Clflags.dump_parc then
      Format.fprintf ppf
        "[%s in mode=%a]@.borrowed=%a@.owned=%a@.bound_funcs=%a@.%a@.@." tag
        Mode.print mode Vset.print borrowed Vset.print owned Vset.print
        bound_funcs Printlambda.lambda expr

  let dump_if ppf expr =
    if !Clflags.dump_parc then (
      Format.fprintf ppf "@.automated_refcounting:@.";
      Printlambda.lambda ppf expr;
      Format.pp_print_newline ppf ());
    expr
end

let empty_env =
  {
    borrowed = Vset.empty;
    owned = Vset.empty;
    matched_expression = None;
    bound_funcs = Vset.empty;
  }

(* Helper functions *)

let ppf = Format.std_formatter

let free_variables { bound_funcs } expr =
  Vset.diff (Lambda.free_variables expr) bound_funcs

let is_matched x = function Some y -> Ident.same x y | None -> false
let map_if pred arg f x = if pred arg then f x else x

(* Perceus algorithm *)
let parc expr =
  let rec parc_helper mode
      ({ borrowed; owned; bound_funcs; matched_expression } as env) expr =
    let regular_if = map_if Mode.is_regular mode in
    let with_dup_if x = regular_if (with_dup x) in
    let with_drop_if x = regular_if (with_drop x) in
    let with_dups_if x = regular_if (with_dups x) in
    let with_drops_if x = regular_if (with_drops x) in
    match expr with
    | Lvar x ->
        (* Rule SVar and SVar-Dup *)
        Logging.log ppf "parc_helper: Lvar" mode env expr;
        let is_rule_svar =
          Vset.equal owned (Vset.singleton x) && not (Vset.mem x borrowed)
        in
        let is_rule_svardup = Vset.is_empty owned && Vset.mem x borrowed in
        let is_bound_func = Vset.mem x bound_funcs in
        if
          Mode.is_borrowed mode || is_bound_func
          || is_matched x matched_expression
          || is_rule_svar
        then expr
        else if is_rule_svardup then with_dup_if x expr
        else raise ParcError
    | Lconst _ ->
        Logging.log ppf "parc_helper: Lconst" mode env expr;
        expr
    | Lapply ({ ap_func; ap_args } as app) ->
        (* Rule SApp, extended to handle multi-argument functions *)
        Logging.log ppf "parc_helper: Lapply" mode env expr;
        (* First, the function is evaluated, then the arguments. So, we run
             [parc_many_left] first on the arguments, and only then run
             [parc_regular] on the function with the resulting environment.
        *)
        let env', ap_args' = parc_many_left Mode.Regular env ap_args in
        let ap_func' = parc_helper Mode.Regular env' ap_func in
        Lapply { app with ap_func = ap_func'; ap_args = ap_args' }
    | Lfunction { kind; params; return; body; attr; loc } ->
        (* Rule SLam and SLam-D *)
        Logging.log ppf "parc_helper: Lfunction" mode env expr;
        let xs = Vset.of_list (List.map fst params) in
        let ys = free_variables env expr in
        let body_fvs = free_variables env body in
        let should_own, should_drop =
          Vset.partition (fun x -> Vset.mem x body_fvs) xs
        in
        let should_dup = Vset.diff ys owned in
        let body' =
          let body'' =
            parc_helper mode
              {
                env with
                borrowed = Vset.empty;
                owned = Vset.union ys should_own;
              }
              body
          in
          (* Insert drops *)
          with_drops_if should_drop body''
        in
        let func' = lfunction ~kind ~params ~return ~body:body' ~attr ~loc in
        (* Insert dups *)
        with_dups_if should_dup func'
    | Llet (_let_kind, value_kind, x, e1, e2) ->
        (* Rule SBind and SBind-D *)
        Logging.log ppf "parc_helper: Llet" mode env expr;
        if Vset.mem x borrowed || Vset.mem x owned then raise ParcError
        else
          let e2_fvs = free_variables env e2 in
          let owned2 = Vset.inter owned (Vset.remove x e2_fvs) in
          let owned' =
            if Vset.mem x e2_fvs then Vset.add x owned2 else owned2
          in
          let e1' =
            parc_helper mode
              {
                env with
                borrowed = Vset.union borrowed owned2;
                owned = Vset.diff owned owned2;
              }
              e1
          in
          let e2' = parc_helper mode { env with borrowed; owned = owned' } e2 in
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
        Logging.log ppf "parc_helper: Lletrec([(x, e1)], e2)" mode env expr;
        if Vset.mem x borrowed || Vset.mem x owned then raise ParcError
        else
          let e2_fvs = free_variables env e2 in
          let owned2 = Vset.inter owned (Vset.remove x e2_fvs) in
          let owned' =
            if Vset.mem x e2_fvs then Vset.add x owned2 else owned2
          in
          let e1' =
            parc_helper mode
              {
                env with
                borrowed = Vset.union borrowed owned2;
                owned = Vset.diff owned owned2;
                bound_funcs = Vset.add x bound_funcs;
              }
              e1
          in
          let e2' =
            parc_helper mode
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
        Logging.log ppf "parc_helper: Lprim(makeblock)" mode env expr;
        Lprim (p, snd (parc_many_right mode env args), loc)
    | Lprim (Pccall desc, _, _)
      when Primitive.native_name desc = get_refcount_native_name
           || Primitive.native_name desc = dup_native_name
           || Primitive.native_name desc = drop_native_name
           || Primitive.native_name desc = dup_copy_native_name ->
        (* Do not perform refcounting on refcounting operations *)
        expr
    | Lprim (p, args, loc) ->
        Logging.log ppf "parc_helper: Lprim(_)" mode env expr;
        let _, args' = parc_many_left Mode.Borrowed env args in
        Lprim (p, args', loc)
    | Lifthenelse (cond, e1, e2) ->
        Logging.log ppf "parc_helper: Lifthenelse" mode env expr;
        let owned_e1 = Vset.inter owned (free_variables env e1) in
        let should_drop_e1 = Vset.diff owned owned_e1 in
        let owned_e2 = Vset.inter owned (free_variables env e2) in
        let should_drop_e2 = Vset.diff owned owned_e2 in
        let e1' =
          parc_helper mode { env with owned = owned_e1 } e1
          |> with_drops_if should_drop_e1
        in
        let e2' =
          parc_helper mode { env with owned = owned_e2 } e2
          |> with_drops_if should_drop_e2
        in
        let cond' =
          parc_helper Mode.Borrowed
            {
              env with
              borrowed = Vset.union (Vset.union borrowed owned_e1) owned_e2;
              owned = Vset.diff (Vset.diff owned owned_e1) owned_e2;
            }
            cond
        in
        Lifthenelse (cond', e1', e2')
    | Lsequence (e1, e2) ->
        let _, exprs = parc_many_right mode env [ e1; e2 ] in
        Lsequence (List.hd exprs, List.hd (List.tl exprs))
    | Lmarker (Match_begin id, lam) when Mode.is_regular mode ->
        Logging.log ppf "parc_helper: Lmarker(Match_begin)" mode env expr;
        let env' =
          {
            env with
            owned = Vset.remove id owned;
            matched_expression = Some id;
          }
        in
        let lam' = parc_helper Mode.Borrowed env' lam in
        Lmarker (Match_begin id, lam')
    | Lmarker (Match_begin _, _) when Mode.is_borrowed mode ->
        Logging.log ppf "parc_borrowed: Lmarker(Match_begin)" mode env expr;
        raise ParcError
    | Lmarker (Matched_body _, _) when Mode.is_regular mode ->
        Logging.log ppf "parc_helper: Lmarker(Matched_body)" mode env expr;
        raise ParcError
    | Lmarker (Matched_body pat, lam)
      when Option.is_some matched_expression && Mode.is_borrowed mode ->
        Logging.log ppf "parc_borrowed: Lmarker(Matched_body)" mode env expr;
        let id = Option.get matched_expression in
        let bv = Vset.of_list (Typedtree.pat_bound_idents pat) in
        let fv = free_variables env lam in
        let owned_bv = Vset.union owned bv in
        let owned' = Vset.inter owned_bv fv in
        let should_drop = Vset.diff owned_bv owned' in
        let e' = parc_helper mode { env with owned = owned' } lam in
        let lam' =
          with_drops_if should_drop e' |> fun lam ->
          if Vset.mem id fv then lam else with_drop_if id lam |> with_dups_if bv
        in
        Lmarker (Matched_body pat, lam')
    | _ ->
        Logging.log ppf "parc_helper: unhandled" mode env expr;
        expr
  (* [parc_helper], but on expression lists

     We can either apply the algorithm left-to-right or right-to-left.
     The choice depends on the expression evaluation order.

     If the eval order if left-to-right, use [parc_many_right]
     If the eval order if right-to-left, use [parc_many_left]
     otherwise, it is not safe to use any of the [parc_many] variants. You
     will have to invoke [parc_helper] manually on each expression.
  *)
  and parc_many_right mode env exprs =
    List.fold_right
      (fun e (env, es) ->
        let env', e' = parc_many_f mode env e in
        (env', e' :: es))
      exprs (env, [])
  and parc_many_left mode env exprs =
    List.fold_left_map (parc_many_f mode) env exprs
  and parc_many_f mode ({ borrowed; owned } as env) e =
    let owned' = Vset.inter owned (free_variables env e) in
    let e' = parc_helper mode { env with owned = owned' } e in
    let env' =
      {
        env with
        borrowed = Vset.union borrowed owned';
        owned = Vset.diff owned owned';
      }
    in
    (env', e')
  in
  Logging.log ppf "parc: begin" Mode.Regular empty_env expr;
  parc_helper Mode.Regular empty_env expr |> Logging.dump_if ppf

let parc_program program =
  let code = parc program.code in
  { program with code }
