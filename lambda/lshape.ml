open Format
open Lambda
open Typedtree

exception MissingChildren

type shape_info =
  | Empty
  | Compound of shape list

and shape = {
  kind: value_kind ;
  name: Ident.t option ;
  info: shape_info option ;
}

and shape_map = shape Ident.Map.t

(* TODO redefine *)
let merge _s1 s2 = s2

let merge_maps =
  Ident.Map.union (fun _ s1 s2 -> Some (merge s1 s2))

let children_of shapes x =
  let ( let* ) = Option.bind in
  match Ident.Map.find_opt x shapes with
  | Some { info } ->
      (match info with
      | Some (Compound shapes') ->
          let maybe_children =
            List.map (fun s -> s.name) shapes'
            |> List.fold_left (fun acc child ->
                let* acc = acc in
                let* child = child in
                Some (child :: acc)) (Some [])
          in
          (match maybe_children with
          | Some children -> Ident.Set.of_list children
          | None -> raise MissingChildren)
      | _ -> Ident.Set.empty)
  | None -> Ident.Set.empty

let rec descendant shapes parent x =
  let ys = children_of shapes parent in
  Ident.Set.mem x ys || Ident.Set.exists (descendant shapes x) ys

let shape_unknown vk = { kind= vk; name= None; info= None }
let int_shape = { kind=Pintval; name= None; info= Some Empty }
let _named_int_shape id = { kind= Pintval; name= id; info= Some Empty }
let gen_shape s = { kind= Pgenval; name= None; info= Some s }
let _named_gen_shape s id = { kind= Pgenval; name= id; info= Some s }
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
      let shape = { subpat_shape with name= Some id } in
      (shape, Ident.Map.add id shape shapes')
  | _ ->
      (* Not handled yet. Return a sound approximation *)
      (gen_shape_unknown, Ident.Map.empty)

let infer_from_matched id pat =
  let id_shape, bv_shapes = infer_from_pattern pat in 
  Ident.Map.add id id_shape bv_shapes

let is_gen_shape_unknown s = (s = gen_shape_unknown)

let is_int { kind; info } =
  match (kind, info) with
  | (Pintval, Some Empty) -> true
  | _ -> false

let is_ptr { kind; info } =
  match (kind, info) with
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

and print_shape ppf ({ kind; name; info } as s) =
  if is_int s || is_gen_shape_unknown s then
    fprintf ppf "%s" (field_kind kind)
  else
    fprintf ppf "@[<2>%a %s::%a@]"
      (pp_print_option ~none:(fun ppf () ->
        fprintf ppf "") Ident.print) name
      (field_kind kind)
      (pp_print_option ~none:(fun ppf () ->
        fprintf ppf "?") print_shape_info) info
