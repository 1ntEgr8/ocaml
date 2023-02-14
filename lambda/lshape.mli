open Lambda
open Format

type shape_info =
  | Empty
  | Simple of value_kind
  | Compound of shape_info list

type shape = value_kind * (shape_info option)

(* val merge : shape -> shape -> shape *)
val infer_from_value_kind : value_kind -> shape
val infer_from_pattern : Typedtree.pattern -> shape

val is_int : shape -> bool
val is_ptr : shape -> bool

val print_shape_info : formatter -> shape_info -> unit

val print : formatter -> shape -> unit
