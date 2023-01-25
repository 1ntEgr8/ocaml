open Clambda

let prim name lam =
  Uprim (Pccall (Primitive.simple
          ~name:name
          ~arity:1
          ~alloc:false
      ), [lam], Debuginfo.none)

let dup = prim "caml_rc_dup"

let drop = prim "caml_rc_drop"
