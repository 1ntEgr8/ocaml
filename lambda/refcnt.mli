(* Wrappers for reference counting primitives *)

open Lambda
open Lshape

val is_rc_op : string -> bool

module type Rc = sig
  val ptr : Ident.t -> lambda
  val checked : Ident.t -> lambda
  val bind_copy: Ident.t -> lambda -> lambda
  val sequence : ?bind_int:bool -> shape_map -> Ident.t -> lambda -> lambda
  val sequence_many : ?bind_int:bool -> shape_map -> Ident.Set.t -> lambda -> lambda
end

module Dup : Rc

module Drop : sig
  include Rc

  val decr : Ident.t -> lambda
  val free : Ident.t -> lambda
  val is_unique : Ident.t -> lambda
end

module Opt : sig
  type dups = Ident.Set.t
  type drops = Ident.Set.t
  type op

  val fuse : dups * drops -> dups * drops
  val combine : dups * drops -> op list
  val specialize_drops: shape_map -> dups * drops -> op list

  val finalize : shape_map -> lambda -> op list -> lambda
end
