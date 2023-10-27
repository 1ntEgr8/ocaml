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
  val special: Ident.t -> lambda -> lambda -> lambda -> lambda
  val reuse_special : Ident.t -> lambda -> lambda -> lambda -> lambda
end

module Opt : sig
  type t

  val init : dups:Ident.Set.t -> drops:Ident.t list -> t
  val fuse : t -> t
  val specialize_drops : shape_map -> t -> t
  val finalize : ?for_matched:bool -> shape_map -> lambda -> t -> lambda
end

val expand : lambda -> lambda
