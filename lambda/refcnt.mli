(* Wrappers for reference counting primitives *)

open Lambda
open Lshape

val dup_native_name : string
val dup : lambda -> lambda
val with_dup : Ident.t -> lambda -> lambda
val with_dup_if : shape Ident.Map.t -> Ident.t -> lambda -> lambda
val with_dups : Ident.Set.t -> lambda -> lambda
val with_dups_if : shape Ident.Map.t -> Ident.Set.t -> lambda -> lambda

val dup_copy_native_name : string
val dup_copy : lambda -> lambda

val drop_native_name : string
val drop : lambda -> lambda
val with_drop : Ident.t -> lambda -> lambda
val with_drop_if: shape Ident.Map.t -> Ident.t -> lambda -> lambda
val with_drops : Ident.Set.t -> lambda -> lambda
val with_drops_if : shape Ident.Map.t -> Ident.Set.t -> lambda -> lambda
