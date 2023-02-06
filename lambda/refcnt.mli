(* Wrappers for reference counting primitives *)

val dup : Lambda.lambda -> Lambda.lambda

val dup_copy : Lambda.lambda -> Lambda.lambda

val drop : Lambda.lambda -> Lambda.lambda
