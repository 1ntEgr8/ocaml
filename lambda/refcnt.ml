open Lambda

let prim name lam =
  Lprim (Pccall (Primitive.simple
          ~name:name
          ~arity:1
          ~alloc:false
      ), [lam], Debuginfo.Scoped_location.Loc_unknown)

let dup = prim "caml_rc_dup"

let dup_copy = prim "caml_rc_copy"

let drop = prim "caml_rc_drop"
