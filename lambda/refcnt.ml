open Lambda
open Lshape

let dup_copy_native_name  = "caml_rc_dup_copy"
let dup_checked_native_name = "caml_rc_dup"
let dup_ptr_native_name = "caml_rc_dup_ptr"
let drop_checked_native_name = "caml_rc_drop"
let drop_ptr_native_name = "caml_rc_drop_ptr"
let decr_native_name = "caml_rc_decr"
let free_native_name = "caml_rc_free"
let is_unique_native_name = "caml_rc_is_unique"
let get_refcount = "caml_obj_get_refcount"

module StringSet = Set.Make(String)

let native_names =
  StringSet.of_list [
    dup_copy_native_name ;
    dup_checked_native_name ;
    dup_ptr_native_name ;
    drop_checked_native_name ;
    drop_ptr_native_name ;
    decr_native_name ;
    free_native_name ;
    is_unique_native_name ;
    get_refcount ;
  ]

let is_rc_op x = StringSet.mem x native_names 

let prim name x =
  Lprim (Pccall (Primitive.simple
          ~name:name
          ~arity:1
          ~alloc:false
      ), [Lvar x], Debuginfo.Scoped_location.Loc_unknown)

module type RcOp = sig
  val ptr : Ident.t -> lambda
  val checked : Ident.t -> lambda
  val bind_copy : Ident.t -> lambda -> lambda
end

module type Rc = sig
  include RcOp

  val sequence : ?bind_int:bool -> shape_map -> Ident.t -> lambda -> lambda
  val sequence_many : ?bind_int:bool -> shape_map -> Ident.Set.t -> lambda -> lambda
end

module MakeRc (R : RcOp) = struct
  include R

  let sequence ?(bind_int = false) shapes x expr =
    try
      let shape = Ident.Map.find x shapes in
      if is_int shape then
        if bind_int then bind_copy x expr else expr
      else if is_ptr shape then
        Lsequence (ptr x, expr)
      else
        Lsequence (checked x, expr)
    with Not_found -> Lsequence (checked x, expr)

  let sequence_many ?(bind_int = false) shapes xs expr =
    Ident.Set.fold (sequence ~bind_int shapes) xs expr
end

module Dup = MakeRc (struct
  let ptr = prim dup_ptr_native_name
  let checked = prim dup_checked_native_name
  let bind_copy x expr =
    let x' = Ident.rename x in
    Llet (Strict, Pintval, x',
          prim dup_copy_native_name x,
          rename (Ident.Map.singleton x x') expr)
end)

module Drop = struct
  include (MakeRc (struct
    let ptr = prim drop_ptr_native_name
    let checked = prim drop_checked_native_name
    let bind_copy _x expr = expr
  end))
  
  let decr = prim decr_native_name
  let free = prim free_native_name
  let is_unique = prim is_unique_native_name
end

module Opt = struct
  type dups = Ident.Set.t

  and drop_op =
    | DropRegular
    | DropInline of { uniq: t ; shared: Ident.Set.t }
  and drops = (drop_op * Ident.t) list

  (* Maintains the invariant where all dups are done before any drops *)
  and t = dups * drops

  open Format

  let ppf = std_formatter

  let rec print ppf (dups, drops) =
    let dups = Ident.Set.elements dups in
    fprintf ppf "@[<v 0>";
    pp_print_list (fun ppf x ->
      fprintf ppf "dup %a" Ident.print x
    ) ppf dups ;
    if (List.length dups > 0) then
      pp_print_cut ppf () ;
    pp_print_list (fun ppf (op, x) ->
      match op with
      | DropRegular -> fprintf ppf "drop %a" Ident.print x
      | DropInline { uniq; shared } ->
          fprintf ppf "@[<v 2>";
          fprintf ppf "drop_inline %a@," Ident.print x ;
          fprintf ppf "(" ;
          fprintf ppf "@[<v 2>uniq:@;%a@]@," print uniq;
          fprintf ppf "@[<v 2>shared:@;%a@]" Ident.Set.print shared;
          fprintf ppf ")" ;
          fprintf ppf "@]"
    ) ppf drops ;
    fprintf ppf "@]"

  let dump_if ppf pass t =
    if !Clflags.dump_parc_opt_trace then (
      fprintf ppf "@.parc_opt(after %s):@." pass;
      print ppf t ;
      pp_print_newline ppf () ;
    );
    t

  let init ~dups ~drops =
    let t = (dups, List.map (fun x -> (DropRegular, x)) drops)
    in
    dump_if ppf "init" t
    
  let fuse (dups, drops) =
    let dups' =
      Ident.Set.diff dups (Ident.Set.of_list (List.map snd drops))
    in
    let drops' =
      List.filter (fun (_, x) -> not (Ident.Set.mem x dups)) drops
    in
    let t = (dups', drops') in
    dump_if ppf "fuse" t

  let specialize_drops shapes t =
    let rec optimize t =
      let (fdups, fdrops) = fuse t in
      optimize_disjoint fdups fdrops
    
    and optimize_disjoint dups drops =
      match drops with
      | [] -> (dups, [])
      | (_, y) :: drops ->
          let (y_dups, dups') =
            Ident.Set.partition (descendant shapes y) dups
          in
          let (y_drops, drops') =
            List.partition ((fun (_, x) -> descendant shapes y x)) drops
          in
          let (rest_dups, rest_drops) = optimize_disjoint dups' drops' in
          let prefix = y_drops in
          let spec = specialize_drop y_dups y in
          (rest_dups, prefix @ [spec] @ rest_drops)

    and specialize_drop dups y =
      let shared = dups in
      let maybe_children = children_of shapes y in
      match maybe_children with
      | None -> (* don't specialize *) (DropRegular, y)
      | Some children ->
        let children =
          Ident.Set.elements children
          |> List.map (fun x -> (DropRegular, x))
        in
        let uniq = optimize (dups, children) in
        (DropInline { uniq; shared }, y)

    in
      dump_if ppf "specialize_drops" (optimize t)

  let rec finalize ?(for_matched = false) shapes lam (dups, drops) =
    let sequence_drop (op, x) lam =
      match op with
      | DropRegular -> Drop.sequence ~bind_int:for_matched shapes x lam
      | DropInline { uniq ; shared } ->
          (* Dups on integer vars inside a matched body must be
             outside the if statement since we need to bind-copy them. They
             must be performed before the parent is dropped.
           *)
          let int_dups, shared =
              let is_int x =
                let shape = Ident.Map.find_opt x shapes in
                match shape with
                | Some shape -> is_int shape
                | _ -> false
              in
              Ident.Set.partition (is_int) shared
          in
          let cond = Drop.is_unique x in
          let e1 = finalize ~for_matched shapes (Drop.free x) uniq in
          let e2 =
            Dup.sequence_many shapes shared @@
            Drop.decr x
          in
          Dup.sequence_many ~bind_int:for_matched shapes int_dups @@
          Lsequence (Lifthenelse (cond, e1, e2), lam)
    in
    Dup.sequence_many ~bind_int:for_matched shapes dups @@
    List.fold_right sequence_drop drops lam
end
