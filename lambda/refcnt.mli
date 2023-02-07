(* Wrappers for reference counting primitives *)

val dup : Lambda.lambda -> Lambda.lambda
val with_dup : Ident.t -> Lambda.lambda -> Lambda.lambda
val with_dups : Ident.Set.t -> Lambda.lambda -> Lambda.lambda

val dup_copy_native_name : string
val dup_copy : Lambda.lambda -> Lambda.lambda

val drop : Lambda.lambda -> Lambda.lambda
val with_drop : Ident.t -> Lambda.lambda -> Lambda.lambda
val with_drops : Ident.Set.t -> Lambda.lambda -> Lambda.lambda
