open Format
open Lambda
open Refcnt

exception ParcError

module Vset = Ident.Set

type env = {
  borrowed : Vset.t;
  owned : Vset.t;
  matched_expression : Ident.t option;
  bound_funcs : Vset.t;
  shapes: Lshape.shape Ident.Map.t;
}

let empty_env =
  {
    borrowed = Vset.empty;
    owned = Vset.empty;
    matched_expression = None;
    bound_funcs = Vset.empty;
    shapes = Ident.Map.empty;
  }

module Logging = struct
  let log ppf tag { borrowed; owned; matched_expression; bound_funcs; shapes } expr =
    if !Clflags.dump_parc_trace then (
      fprintf ppf "[%s]@." tag ;
      fprintf ppf "borrowed=%a@." Vset.print borrowed ;
      fprintf ppf "owned=%a@." Vset.print owned ;
      fprintf ppf "matched_expression=%a@."
        (pp_print_option Ident.print) matched_expression ;
      fprintf ppf "bound_funcs=%a@." Vset.print bound_funcs ;
      fprintf ppf "shapes={@[@;" ;
      Ident.Map.iter (fun id shape ->
        fprintf ppf "@[<2>%a =>@ %a@]@ ;@ @;"
          Ident.print id
          Lshape.print_shape shape
      ) shapes ;
      fprintf ppf "}@]@." ;
      fprintf ppf "%a@.@." Printlambda.lambda expr
    )

  let dump_if ppf expr =
    if !Clflags.dump_parc then (
      fprintf ppf "@.automated_refcounting:@.";
      Printlambda.lambda ppf expr ;
      pp_print_newline ppf ();
    );
    expr
end

let ppf = Format.std_formatter

let free_variables { bound_funcs } expr =
  Vset.diff (Lambda.free_variables expr) bound_funcs

let map_if flag f x =
  if flag then f x else x

let parc expr =
  let rec parc_regular ({ borrowed; owned; bound_funcs; matched_expression; shapes } as env) expr =
    match expr with
    | Lvar x ->
        (* Rule SVar and SVar-Dup *)
        Logging.log ppf "parc_helper: Lvar" env expr;
        let is_matched =
          match matched_expression with
          | Some y -> Ident.same x y
          | None -> false
        in
        if Vset.mem x bound_funcs || is_matched then expr
        else if Vset.is_empty owned && Vset.mem x borrowed then
          Dup.sequence shapes x expr
        else if Vset.equal owned (Vset.singleton x) && not (Vset.mem x borrowed)
        then expr
        else raise ParcError
    | Lconst _ ->
        Logging.log ppf "parc_helper: Lconst" env expr;
        expr
    | Lapply ({ ap_func; ap_args } as app) ->
        (* Rule SApp

           Extended to handle multi-argument functions.
         *)
        Logging.log ppf "parc_helper: Lapply" env expr;

        (* First, the function is evaluated, then the arguments. So, we run
           [parc_many_left] first on the arguments, and only then run
           [parc_regular] on the function with the resulting environment.
         *)
        let env', ap_args' = parc_many_left env ap_args in
        let ap_func' = parc_regular env' ap_func in
        Lapply { app with ap_func = ap_func'; ap_args = ap_args' }
    | Lfunction { kind; params; return; body; attr; loc } ->
        Logging.log ppf "parc_helper: Lfunction" env expr;
        (* Rule SLam and SLam-D *)
        let xs = Vset.of_list (List.map fst params) in
        let ys = free_variables env expr in
        let shapes' =
          List.fold_left (fun shapes (id, vk) ->
            Ident.Map.add id (Lshape.infer_from_value_kind ~name:id vk) shapes   
          ) shapes params
        in
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
                shapes = shapes';
              }
              body
          in
          (* Insert drops *)
          Drop.sequence_many shapes should_drop body''
        in
        let func' = lfunction ~kind ~params ~return ~body:body' ~attr ~loc in
        (* Insert dups *)
        Dup.sequence_many shapes should_dup func'
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
          let e2' =
            parc_regular
              { 
                env with
                borrowed;
                owned = owned';
                shapes = Ident.Map.add x (Lshape.infer_from_value_kind ~name:x value_kind) shapes
              }
              e2
          in

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
        Lprim (p, snd (parc_many_left env args), loc)
    | Lprim ((Pccall desc), _, _) when is_rc_op (Primitive.native_name desc) ->
        expr
    (* TODO(1ntEgr8): Might need to handle certain primitives differently (like
       array primitives) *)
    | Lprim (p, args, loc) ->
      Logging.log ppf "parc_helper: Lprim(_)" env expr;
      let _, args' = parc_many_left env args in
      Lprim (p, args', loc)
    | Lifthenelse (cond, e1, e2) ->
        let owned_e1 = Vset.inter owned (free_variables env e1) in
        let should_drop_e1 = Vset.diff owned owned_e1 in
        let owned_e2 = Vset.inter owned (free_variables env e2) in
        let should_drop_e2 = Vset.diff owned owned_e2 in
        let e1' =
          Drop.sequence_many shapes should_drop_e1 @@
          parc_regular { env with owned= owned_e1 } e1
        in
        let e2' =
          Drop.sequence_many shapes should_drop_e2 @@
          parc_regular { env with owned= owned_e2 } e2
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
        Lifthenelse (cond', e1', e2')
    | Lsequence (e1, e2) ->
        let _, exprs = parc_many_right env [ e1; e2 ] in
        Lsequence (List.hd exprs, List.hd (List.tl exprs))
    | Lmarker (Match_begin id, lam) ->
        Logging.log ppf "parc_helper: Lmarker(Match_begin)" env expr;
        let env' =
          {
            env with
            owned = Vset.remove id owned;
            matched_expression = Some id;
            shapes = Ident.Map.remove id shapes;
          }
        in
        let lam' = parc_match env' lam in
        Lmarker (Match_begin id, lam')
    | Lmarker (Matched_body _, _) ->
        Logging.log ppf "parc_helper: Lmarker(Matched_body)" env expr;
        raise ParcError
    | _ ->
        Logging.log ppf "parc_helper: unhandled" env expr;
        expr
    and parc_match ({ owned; matched_expression; shapes } as env) expr =
    (* TODO(1ntEgr8): Need to update this to handle guards *)
    match expr with
    | Lmarker (Match_begin _, _) ->
        Logging.log ppf "parc_match: Lmarker(Match_begin)" env expr;
        raise ParcError
    | Lmarker (Matched_body pat, lam) when Option.is_some matched_expression ->
        Logging.log ppf "parc_match: Lmarker(Matched_body)" env expr;
        let id = Option.get matched_expression in
        let bv = Vset.of_list (Typedtree.pat_bound_idents pat) in
        let fv = free_variables env lam in
        let owned_bv = Vset.union owned bv in
        let owned' = Vset.inter owned_bv fv in
        let shapes' = Lshape.merge_maps shapes (Lshape.infer_from_matched id pat) in
        let should_dup = bv in
        let should_drop =
          let base = Vset.diff owned_bv owned' in
          let base' =
            if not (Vset.mem id fv) then
              Vset.add id base
            else
              base
          in
            Vset.elements base'
        in
        let lam' =
          Opt.init ~dups:should_dup ~drops:should_drop
          |> map_if !Clflags.specialize_drops (Opt.specialize_drops shapes')
          |> Opt.finalize
              ~for_matched:true
              shapes'
              (parc_regular { env with owned= owned'; shapes= shapes' } lam)
        in
        Lmarker (Matched_body pat, lam')
    | Lmarker (Matched_body _, _) when Option.is_none matched_expression ->
        raise ParcError
    | _ ->
        Logging.log ppf "parc_match: catch all" env expr;
        shallow_map (fun e -> parc_match env e) expr

  (* [parc], but on expression lists

     We can either apply the algorithm left-to-right or right-to-left.
     The choice depends on the expression evaluation order.

     If the eval order if left-to-right, use [parc_many_right]
     If the eval order if right-to-left, use [parc_many_left]
     otherwise, it is not safe to use any of the [parc_many] variants; You
     will have to invoke [parc] manually on each expression.
  *)
  and parc_many_right env exprs =
    List.fold_right (fun e (env, es) ->
      let (env', e') = parc_many_f env e in
      (env', e' :: es)
    ) exprs (env, [])
  and parc_many_left env exprs =
    List.fold_left_map parc_many_f env exprs
  and parc_many_f ({ borrowed; owned } as env) e =
    let owned' = Vset.inter owned (free_variables env e) in
    let e' = parc_regular { env with owned = owned' } e in
    let env' =
      {
        env with
        borrowed = Vset.union borrowed owned';
        owned = Vset.diff owned owned';
      }
    in
    (env', e')
 in
  Logging.log ppf "parc: begin" empty_env expr;
  parc_regular empty_env expr |> Logging.dump_if ppf

let parc_program program =
  let code = parc program.code in
  { program with code }
