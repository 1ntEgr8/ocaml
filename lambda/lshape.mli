open Lambda
open Format

type shape_info =
  | Empty
  | Compound of shape_info list

type shape = shape_info option

val merge : shape -> shape -> shape
val infer_from_value_kind : value_kind -> shape
val infer_from_pattern : Ident.t -> Typedtree.pattern -> shape Ident.Map.t

val is_int : shape -> bool
val is_ptr : shape -> bool

val print_shape_info : formatter -> shape_info -> unit

val print : formatter -> shape -> unit
