open Lambda
open Format

type shape_info =
  | Empty
  | Compound of shape list

and shape = {
  kind: value_kind ;
  name: Ident.t option ;
  info: shape_info option ;
}

and shape_map = shape Ident.Map.t

val merge : shape -> shape -> shape
val merge_maps : shape Ident.Map.t -> shape Ident.Map.t -> shape Ident.Map.t
val descendant : shape_map -> Ident.t -> Ident.t -> bool
val infer_from_value_kind : value_kind -> shape
val infer_from_pattern : Typedtree.pattern -> shape * (shape Ident.Map.t)
val infer_from_matched: Ident.t -> Typedtree.pattern -> shape Ident.Map.t

val is_int : shape -> bool
val is_ptr : shape -> bool

val print_shape_info : formatter -> shape_info -> unit

val print_shape : formatter -> shape -> unit
