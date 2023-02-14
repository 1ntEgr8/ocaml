open Format
open Lambda
open Typedtree

type shape_info =
  | Empty
  | Simple of value_kind
  | Compound of shape_info list

type shape = value_kind * (shape_info option)

let infer_from_value_kind vk =
  match vk with
  | Pintval -> (Pintval, Some Empty)
  | _ -> (vk, None)

let infer_from_pattern pat =
  let vk = Typeopt.value_kind pat.pat_env pat.pat_type in
  (vk, None)

let is_int (vk, _) =
  match vk with
  | Pintval -> true
  | _ -> false

let is_ptr s =
  match s with
  | Pgenval, Some s -> s <> Empty
  | _ -> false

let boxed_integer_name = function
  | Pnativeint -> "nativeint"
  | Pint32 -> "int32"
  | Pint64 -> "int64"

let field_kind = function
  | Pgenval -> "*"
  | Pintval -> "int"
  | Pfloatval -> "float"
  | Pboxedintval bi -> boxed_integer_name bi

let rec print_shape_info ppf = function
  | Empty -> fprintf ppf "()"
  | Simple vk -> fprintf ppf "%s" (field_kind vk)
  | Compound sis ->
      fprintf ppf "@[<2>(" ;
      List.iter (print_shape_info ppf) sis ;
      fprintf ppf ")@]"

let print ppf (vk, si) =
  fprintf ppf "@[<2>%s::<%a>@]"
    (field_kind vk)
    (pp_print_option ~none:(fun ppf () ->
      fprintf ppf "unknown") print_shape_info) si
