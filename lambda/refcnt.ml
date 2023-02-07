open Lambda

let prim name lam =
  Lprim (Pccall (Primitive.simple
          ~name:name
          ~arity:1
          ~alloc:false
      ), [lam], Debuginfo.Scoped_location.Loc_unknown)

let dup = prim "caml_rc_dup"
let with_dup x expr = Lsequence (dup (Lvar x), expr)
let with_dups xs expr = Ident.Set.fold (fun x acc -> with_dup x acc) xs expr

let dup_copy_native_name = "caml_rc_copy"
let dup_copy = prim dup_copy_native_name

let drop = prim "caml_rc_drop"
let with_drop x expr = Lsequence (drop (Lvar x), expr)
let with_drops xs expr = Ident.Set.fold (fun x acc -> with_drop x acc) xs expr

