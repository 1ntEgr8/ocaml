open Lambda
open Typedtree

module IntMap = Map.Make(struct
  type t = int
  let compare = Stdlib.compare
end)

type _env = {
  mutable available: (reuse_info list) IntMap.t;     (* size => [reuse_info] *)
  mutable reused: Ident.Set.t;
  mutable deconstructed: (pattern option) Ident.Map.t;
}

let empty_env = {
  available= IntMap.empty ;
  reused= Ident.Set.empty ;
  deconstructed= Ident.Map.empty ;
}

let size_of _block_shape = 0

let ru env expr =
  let try_reuse expr =
    match expr with
    | Lprim (Pmakeblock (_, _, block_shape), _, _) ->
      let size = size_of block_shape in
      (match IntMap.find_opt size env.available with
      | Some (info :: infos) ->
        env.available <- IntMap.add size infos env.available ;
        env.reused <- Ident.Set.add info env.reused ;
        alloc_at info expr
      | _ -> expr)
    | _ -> expr
  in
  try_reuse expr

let insert_reuse_tokens program =
  { program with code= ru empty_env program.code }

(* TODO:(1ntEgr8): Ignore for reuse specialization, will implement later *)

let ru_spec expr = expr

let specialize_reuses program =
  { program with code= ru_spec program.code }
