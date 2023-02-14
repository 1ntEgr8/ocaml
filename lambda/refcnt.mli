(* Wrappers for reference counting primitives *)

open Lambda
open Lshape

val is_rc_op : string -> bool

module type Rc = sig
  val ptr : Ident.t -> lambda
  val checked : Ident.t -> lambda
  val sequence : shape Ident.Map.t -> Ident.t -> lambda -> lambda
  val sequence_many : shape Ident.Map.t -> Ident.Set.t -> lambda -> lambda
end

module Dup : Rc

module Drop : sig
  include Rc

  val decr : Ident.t -> lambda
  val free : Ident.t -> lambda
  val is_unique : Ident.t -> lambda
end
