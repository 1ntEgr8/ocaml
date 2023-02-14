open Format
open Lambda

type shape_info =
  | Empty
  | Simple of value_kind
  | Compound of shape_info list

type shape = value_kind * (shape_info option)

let infer_from_value_kind vk =
  match vk with
  | Pintval -> (Pintval, Some Empty)
  | _ -> (vk, None)

let should_refcount (vk, _) =
  match vk with
  | Pgenval -> true
  | _ -> false

let rec print_shape_info ppf = function
  | Empty -> fprintf ppf "()"
  | Simple vk -> fprintf ppf "%a" Printlambda.value_kind vk
  | Compound sis ->
      fprintf ppf "@[<2>(" ;
      List.iter (print_shape_info ppf) sis ;
      fprintf ppf ")@]"

let print ppf (vk, si) =
  fprintf ppf "@[<2>value_kind: %a, shape_info: %a@]"
    Printlambda.value_kind vk
    (pp_print_option print_shape_info) si
