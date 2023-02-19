open Format
open Lambda
open Typedtree

(* TODO
  
  - define [merge]
  - alias all sub-patterns
  - add names to shape
    - define children_of
    - define descendent_of
 *)


type shape_info =
  | Empty
  | Compound of shape list

and shape = value_kind * (shape_info option)
and shape_map = shape Ident.Map.t

(* TODO redefine *)
let merge _s1 s2 = s2

let merge_maps =
  Ident.Map.union (fun _ s1 s2 -> Some (merge s1 s2))

(* TODO redefine *)
let descendant _shapes _parent _x = false

let shape_unknown vk = (vk, None)
let int_shape = (Pintval, Some Empty)
let gen_shape s = (Pgenval, Some s)
let gen_shape_unknown = shape_unknown Pgenval

let infer_from_value_kind vk =
  match vk with
  | Pintval -> int_shape
  | _ -> shape_unknown vk

let rec infer_from_pattern pat =
  let open Types in
  match pat.pat_desc with
  | Tpat_any -> (gen_shape_unknown, Ident.Map.empty)
  | Tpat_var (id, _) ->
      let vk = Typeopt.value_kind pat.pat_env pat.pat_type in
      let shape = infer_from_value_kind vk in
      (shape, Ident.Map.singleton id shape)
  | Tpat_constant _ -> (int_shape, Ident.Map.empty)
  | Tpat_construct (_, cstr, patl, _) ->
      (match cstr.cstr_tag with
      | Cstr_constant _ | Cstr_unboxed -> (int_shape, Ident.Map.empty)
      | Cstr_block _ ->
          let inferred = List.map infer_from_pattern patl in
          let ss = List.map fst inferred in
          let shape_map =
            List.map snd inferred
            |> List.fold_left merge_maps Ident.Map.empty
          in
          (gen_shape (Compound ss), shape_map)
      | _ -> (gen_shape_unknown, Ident.Map.empty))
  | Tpat_alias (subpat, id, _) ->
      let subpat_shape, shapes' = infer_from_pattern subpat in
      (subpat_shape, Ident.Map.add id subpat_shape shapes')
  | _ ->
      (* Not handled yet. Return a sound approximation *)
      (gen_shape_unknown, Ident.Map.empty)

let infer_from_matched id pat =
  let id_shape, bv_shapes = infer_from_pattern pat in 
  Ident.Map.add id id_shape bv_shapes

let is_gen_shape_unknown s = (s = gen_shape_unknown)

let is_int s =
  match s with
  | (Pintval, Some Empty) -> true
  | _ -> false

let is_ptr s =
  match s with
  | (Pgenval, Some s) -> s <> Empty
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
  | Empty -> fprintf ppf "<empty>"
  | Compound ss ->
      fprintf ppf "@[<2>[" ;
      pp_print_list
        ~pp_sep:(fun ppf () -> pp_print_text ppf ",")
        print_shape
        ppf
        ss
      ;
      fprintf ppf "]@]"

and print_shape ppf ((vk, si) as s) =
  if is_int s || is_gen_shape_unknown s then
    fprintf ppf "%s" (field_kind vk)
  else
    fprintf ppf "@[<2>%s::%a@]"
      (field_kind vk) 
      (pp_print_option ~none:(fun ppf () ->
        fprintf ppf "?") print_shape_info) si
