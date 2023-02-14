open Lambda

let prim name lam =
  Lprim (Pccall (Primitive.simple
          ~name:name
          ~arity:1
          ~alloc:false
      ), [lam], Debuginfo.Scoped_location.Loc_unknown)

let with_rc_if f shapes x expr =
  try
    let shape = Ident.Map.find x shapes in
    if Lshape.should_refcount shape then
      f x expr
    else
      expr
  with Not_found -> f x expr

(*
let with_rc f_ptr f_checked shapes x expr =
  try
    let shape = Ident.Map.find x shapes in
    if should_refcount shape then
      f_ptr x expr
    else
      expr
  with Not_found -> f_checked x expr
*)


let with_rcs f xs expr = Ident.Set.fold (fun x acc -> f x acc) xs expr
let with_rcs_if f shapes xs expr = Ident.Set.fold (fun x acc -> f shapes x acc) xs expr

let dup_native_name = "caml_rc_dup"
let dup = prim dup_native_name
let with_dup x expr = Lsequence (dup (Lvar x), expr)
let with_dup_if = with_rc_if with_dup
let with_dups = with_rcs with_dup
let with_dups_if = with_rcs_if with_dup_if

let dup_copy_native_name = "caml_rc_copy"
let dup_copy = prim dup_copy_native_name

let drop_native_name = "caml_rc_drop"
let drop = prim drop_native_name
let with_drop x expr = Lsequence (drop (Lvar x), expr)
let with_drop_if = with_rc_if with_drop
let with_drops = with_rcs with_drop
let with_drops_if = with_rcs_if with_drop_if
