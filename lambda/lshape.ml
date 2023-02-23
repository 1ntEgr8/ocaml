open Format
open Lambda
open Typedtree

type shape_error =
  | MissingChildren
  | MergeMismatch

exception ShapeError of shape_error

type shape_info =
  | Empty
  | Compound of shape list

and shape = {
  kind: value_kind ;
  name: Ident.t option ;
  info: shape_info option ;
}

and shape_map = shape Ident.Map.t

let ( let* ) = Option.bind

let merge_opt f o1 o2 =
  match (o1, o2) with
  | Some x, None | None, Some x -> Some x
  | None, None -> None
  | Some x1, Some x2 -> Some (f x1 x2)

let rec merge
  { kind= k1; name= n1; info= i1 }
  { kind= k2; name= n2; info= i2 }
  =
    { kind= merge_kind k1 k2
    ; name= merge_name n1 n2
    ; info= merge_info i1 i2 }

and merge_kind k1 k2 =
  if k1 = k2 then k1 else raise (ShapeError MergeMismatch)

and merge_info i1 i2 =
  merge_opt (fun i1 i2 ->
    match (i1, i2) with
    | Empty, Empty -> Empty
    | Compound c1, Compound c2 ->
        Compound (List.combine c1 c2
                  |> List.map (fun (s1, s2) -> merge s1 s2))
    | _ -> raise (ShapeError MergeMismatch)) i1 i2

and merge_name x1 x2 =
  merge_opt (fun x1 x2 ->
    if Ident.compare x1 x2 = 0 then x1
    else raise (ShapeError MergeMismatch)) x1 x2

let merge_maps =
  Ident.Map.union (fun _ s1 s2 -> Some (merge s1 s2))

let children_of shapes x =
  match Ident.Map.find_opt x shapes with
  | Some { info } ->
      (match info with
      | Some (Compound shapes') ->
          List.map (fun s -> s.name) shapes'
          |> List.fold_left
              (fun acc child ->
                let* acc = acc in
                let* child = child in
                Some (Ident.Set.add child acc))
              (Some Ident.Set.empty)
      | _ -> None)
  | None -> None

let rec descendant shapes ~parent x =
  let ys = children_of shapes parent in
  match ys with
  | None -> false
  | Some ys ->
    Ident.Set.mem x ys || Ident.Set.exists (fun y -> descendant shapes ~parent:y x) ys

let shape vk name info = { kind=vk; name; info }
let int_shape name = { kind=Pintval; name; info= Some Empty }
let gen_shape name info = { kind= Pgenval; name; info }

let infer_from_value_kind ?name vk =
  match vk with
  | Pintval -> int_shape name
  | _ -> shape vk name None

let rec infer_from_pattern ?name pat =
  let open Types in
  match pat.pat_desc with
  | Tpat_any -> (gen_shape name None, Ident.Map.empty)
  | Tpat_var (id, _) ->
      let vk = Typeopt.value_kind pat.pat_env pat.pat_type in
      let shape = infer_from_value_kind ~name:id vk in
      (shape, Ident.Map.singleton id shape)
  | Tpat_constant _ -> (int_shape name, Ident.Map.empty)
  | Tpat_construct (_, cstr, patl, _) ->
      (match cstr.cstr_tag with
      | Cstr_constant _ | Cstr_unboxed -> (int_shape name, Ident.Map.empty)
      | Cstr_block _ ->
          let inferred = List.map (fun pat -> infer_from_pattern pat) patl in
          let ss = List.map fst inferred in
          let shape_map =
            List.map snd inferred
            |> List.fold_left merge_maps Ident.Map.empty
          in
          (gen_shape name (Some (Compound ss)), shape_map)
      | _ -> (gen_shape name None, Ident.Map.empty))
  | Tpat_alias (subpat, id, _) ->
      let shape, shapes' = infer_from_pattern ~name:id subpat in
      (shape, Ident.Map.add id shape shapes')
  | _ ->
      (* Not handled yet. Return a sound approximation *)
      (gen_shape name None, Ident.Map.empty)

let infer_from_matched id pat =
  let id_shape, bv_shapes = infer_from_pattern ~name:id pat in 
  Ident.Map.add id id_shape bv_shapes

let is_gen_shape_unknown { kind; info } =
  match (kind, info) with
  | (Pgenval, None) -> true
  | _ -> false

let is_int { kind; info } =
  match (kind, info) with
  | (Pintval, Some Empty) -> true
  | _ -> false

let is_ptr { kind; info } =
  match (kind, info) with
  | (Pgenval, Some s) -> s <> Empty
  | _ -> false

let info_is_unknown { info } = Option.is_none info

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
  let print_id_opt =
    pp_print_option ~none:(fun ppf () -> fprintf ppf "?") Ident.print
  in
  if is_int s || is_gen_shape_unknown s then
    fprintf ppf "%a(%s)"
      print_id_opt name
      (field_kind kind)
  else
    fprintf ppf "@[<2>%a(%s:%a)@]"
      print_id_opt name
      (field_kind kind)
      (pp_print_option ~none:(fun ppf () ->
        fprintf ppf "?") print_shape_info) info
