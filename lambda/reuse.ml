open Lambda

type reuse_info = Ident.t

module IntMap = Map.Make(struct
  type t = int
  let compare = Stdlib.compare
end)

type _env = {
  mutable available: (reuse_info list) IntMap.t;     (* size => [reuse_info] *)
  mutable reused: Ident.Set.t;
}

let empty_env = {
  available= IntMap.empty ;
  reused= Ident.Set.empty ;
}

let size_of _block_shape = 0

let ru env expr =
  let try_reuse expr =
    match expr with
    | Lprim (Pmakeblock (tag, mflag, block_shape), args, loc) ->
      let size = size_of block_shape in
      (match IntMap.find_opt size env.available with
      | Some (info :: infos) ->
        env.available <- IntMap.add size infos env.available ;
        env.reused <- Ident.Set.add info env.reused ;
        (* TODO(1ntEgr8): make alloc_at *)
        Lprim (Pmakeblock (tag, mflag, block_shape), args, loc)
      | _ -> expr)
    | _ -> expr
  in
  match expr with
  | Lprim(Pmakeblock _, _, _) ->
      try_reuse expr
  | _ -> expr

let insert_reuse_tokens program =
  { program with code= ru empty_env program.code }


(* TODO:(1ntEgr8): Ignore for reuse specialization, will implement later *)

let ru_spec expr = expr

let specialize_reuses program =
  { program with code= ru_spec program.code }
