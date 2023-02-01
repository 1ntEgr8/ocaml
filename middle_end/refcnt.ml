open Lambda

let prim name lam =
  Lprim (Pccall (Primitive.simple
          ~name:name
          ~arity:1
          ~alloc:false
      ), [lam], Debuginfo.Scoped_location.Loc_unknown)

let dup = prim "caml_rc_dup"

let drop = prim "caml_rc_drop"
