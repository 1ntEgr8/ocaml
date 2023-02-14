open Format
open Lambda
open Typedtree

type shape_info =
  | Empty
  | Compound of shape_info list

type shape = shape_info option

(* TODO redefine *)
let merge _s1 s2 = s2

let dummy_shape = Compound []

let infer_from_value_kind vk =
  match vk with
  | Pintval -> Some Empty
  | _ -> None

let infer_from_pattern id pat =
  let open Types in
  let bvs = pat_bound_idents_full pat in
  let shapes =
    List.fold_left (fun shapes (id, _, ty) ->
      let vk = Typeopt.value_kind pat.pat_env ty in
      let shape' = infer_from_value_kind vk in
      Ident.Map.add id shape' shapes
    ) Ident.Map.empty bvs
  in
  let id_shape =
    match pat.pat_desc with
    | Tpat_construct (_, cstr, _, _) ->
      (match cstr.cstr_tag with
      | Cstr_constant _ | Cstr_unboxed -> Some Empty
      | Cstr_block _ -> Some dummy_shape
      | _ -> None)
    | Tpat_tuple _ -> Some dummy_shape
    | _ -> None
  in
  Ident.Map.add id id_shape shapes

let is_int s =
  match s with
  | Some Empty -> true
  | _ -> false

let is_ptr s =
  match s with
  | Some s -> s <> Empty
  | _ -> false

(*
let boxed_integer_name = function
  | Pnativeint -> "nativeint"
  | Pint32 -> "int32"
  | Pint64 -> "int64"

let field_kind = function
  | Pgenval -> "*"
  | Pintval -> "int"
  | Pfloatval -> "float"
  | Pboxedintval bi -> boxed_integer_name bi
*)

let rec print_shape_info ppf = function
  | Empty -> fprintf ppf "int"
  | Compound sis ->
      fprintf ppf "@[<2>[" ;
      List.iter (print_shape_info ppf) sis ;
      fprintf ppf "]@]"

let print ppf si =
  fprintf ppf "@[<2>%a@]"
    (pp_print_option ~none:(fun ppf () ->
      fprintf ppf "unknown") print_shape_info) si
