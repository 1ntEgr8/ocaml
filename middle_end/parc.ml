open Clambda

exception ParcError

module V = Backend_var

type env = {
  borrowed: V.Set.t;
  owned: V.Set.t;
}

let forget_provenance = Backend_var.With_provenance.var

let rec free_variables = function
  | Uvar id -> V.Set.singleton id
  | Uconst _ | Uunreachable -> V.Set.empty
  | Udirect_apply (_lbl, args, _dbg) ->
      free_variables_list args
  | Ugeneric_apply (fn, args, _dbg) ->
      V.Set.union (free_variables fn)
                  (free_variables_list args)
  | Uclosure (_clos, fvs) ->
      free_variables_list fvs
  | Uoffset (lam, _) -> free_variables lam
  | Ulet (_flag, _k, v, arg, body) ->
    V.Set.union
      (free_variables arg)
      (V.Set.remove (forget_provenance v) (free_variables body))
  | Uphantom_let _ ->
      (* We shouldn't set a phantom let in Clambda *)
      raise ParcError 
  | Uletrec (decl, body) ->
      V.Set.diff
        (V.Set.union
          (free_variables body)
          (free_variables_list (List.map snd decl)))
        (V.Set.of_list (List.map (fun d -> forget_provenance (fst d)) decl))
  | Uprim (_prim, args, _db) -> free_variables_list args
  | Uswitch (arg, sw, _dbg) ->
      V.Set.union
        (V.Set.union
          (free_variables arg)
          (free_variables_list (Array.to_list sw.us_actions_consts)))
        (free_variables_list (Array.to_list sw.us_actions_blocks))
  | Ustringswitch (arg, cases, default) ->
      let set =
        V.Set.union
          (free_variables arg)
          (free_variables_list (List.map snd cases))
      in
      begin match default with
      | None -> set
      | Some default -> V.Set.union set (free_variables default)
      end
  | Ustaticfail (_, args) -> free_variables_list args
  | Ucatch (_, vars, body, handler) ->
    V.Set.union
      (V.Set.diff
        (free_variables handler)
        (V.Set.of_list (List.map (fun d -> forget_provenance (fst d)) vars)))
      (free_variables body)
  | Utrywith (body, param, handler) ->
      V.Set.union
        (free_variables body)
        (V.Set.remove (forget_provenance param) (free_variables handler))
  | Uifthenelse (cond, tru, fls) ->
      V.Set.union
        (V.Set.union (free_variables cond) (free_variables tru))
        (free_variables fls)
  | Usequence (e1, e2) ->
      V.Set.union (free_variables e1) (free_variables e2)
  | Uwhile (cond, body) ->
      V.Set.union (free_variables cond) (free_variables body)
  | Ufor (v, lo, hi, _dir, body) ->
      V.Set.union 
        (V.Set.union (free_variables lo) (free_variables hi))
        (V.Set.remove (forget_provenance v) (free_variables body))
  | Uassign (id, lam) ->
      V.Set.add id (free_variables lam)
  | Usend (_k, met, obj, args, _dbg) ->
      V.Set.union
        (V.Set.union (free_variables met) (free_variables obj))
        (free_variables_list args)
  | Umarker (_, lam) -> free_variables lam
and free_variables_list exprs =
  List.fold_left (fun acc arg -> V.Set.union acc
    (free_variables arg)) V.Set.empty exprs

let parc expr =
  let rec parc_helper expr ({ borrowed; owned } as env) =
    let _is_owned x = V.Set.mem x owned in
    let is_borrowed x = V.Set.mem x borrowed in
    (* TODO add assertion to check invariant *)
    match expr with
    | Uvar x -> begin
      if V.Set.is_empty owned && is_borrowed x then begin
        (* Rule SVar-Dup *)
        Usequence(Refcnt.dup expr, expr)
      end else if V.Set.equal owned (V.Set.singleton x) && not (is_borrowed x) then begin
        (* Rule SVar *)
        expr
      end else begin
        (* ERROR *)
        raise ParcError
      end
    end
    | Uconst _ | Uunreachable -> expr
    | Ugeneric_apply (fn, args, dbg) -> begin
      (* Rule SApp *)
      let (fvs, args') =
        List.fold_left_map (fun fvs arg ->
          let fvs' = free_variables arg in
          let owned' = V.Set.inter owned (free_variables arg) in
          (V.Set.union fvs fvs', parc_helper arg { env with owned= owned'})
        ) V.Set.empty args
      in
      let owned' = V.Set.inter owned fvs in
      let fn' = parc_helper fn { borrowed= V.Set.union borrowed owned'
                               ; owned= V.Set.diff owned owned'} in
      Ugeneric_apply (fn', args', dbg)
    end
    | Umarker (_info, expr') ->
        (* TODO use marker_info *)
        parc_helper expr' env
    | _ -> expr
  in
  parc_helper expr { borrowed= V.Set.empty
                   ; owned= V.Set.empty }
