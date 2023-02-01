open Lambda

exception ParcError

module Vset = Ident.Set

type env = {
  borrowed: Vset.t;
  owned: Vset.t;
}

let empty_env = {
  borrowed= Vset.empty ;
  owned= Vset.empty ;
}

let parc expr = 
  let rec parc_helper ({ borrowed; owned } as env) expr =
    let is_borrowed x = Vset.mem x borrowed in
    match expr with
    | Lvar x -> begin
      if Vset.is_empty owned && is_borrowed x then begin
        (* Rule SVar-Dup *)
        Lsequence(Refcnt.dup expr, expr)
      end else if Vset.equal owned (Vset.singleton x) && not (is_borrowed x) then begin
        (* Rule SVar *)
        expr
      end else begin
        (* ERROR *)
        raise ParcError
      end
    end
    | Lconst _ -> expr
    | Lapply ( { ap_func; ap_args } as app) -> begin
      (* Rule SApp 
         
         Extended to handle multi-argument functions. Assumes left-to-right
         evaluation order.

         TODO(1ntEgr8): Do we have to do anything special to ensure
         left-to-right evaluation order is a guaranteed?
       *)
      let (env', ap_args') =
        List.fold_left_map (fun ({ borrowed; owned } as env) e ->
          let owned' = Vset.inter owned (free_variables e) in
          let e' = parc_helper { env with owned= owned' } e in
          let env' = {
            borrowed= Vset.union borrowed owned';
            owned= Vset.diff owned owned';
          } in
          (env', e')
        ) env ap_args
      in
      let ap_func' = parc_helper env' ap_func in
      Lapply { app with ap_func= ap_func'; ap_args= ap_args' }
    end
    | _ -> expr
  in
    parc_helper empty_env expr
