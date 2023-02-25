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

  module Logging = struct
    open Format

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

    let pre_dump ppf pass t =
      if !Clflags.dump_parc_opt_trace then (
        fprintf ppf "@[<v 2>@;@[<v 0>parc_opt(before: %s):@,%a@;"
          pass
          print t ;
      )

    let post_dump ppf pass t =
      if !Clflags.dump_parc_opt_trace then (
        fprintf ppf "@,parc_opt(after: %s):@,%a@]@]@;"
          pass
          print t ;
      )

    let init ppf () =
      if !Clflags.dump_parc_opt_trace then (
        fprintf ppf "@.parc_opt_trace:@."
      )
  end

  let ppf = Format.std_formatter

  let init ~dups ~drops =
    Logging.init ppf () ;
    (dups, List.map (fun x -> (DropRegular, x)) drops)
    
  let fuse ((dups, drops) as t) =
    Logging.pre_dump ppf "fuse" t ;

    let dups' =
      Ident.Set.diff dups (Ident.Set.of_list (List.map snd drops))
    in
    let drops' =
      List.filter (fun (_, x) -> not (Ident.Set.mem x dups)) drops
    in
    let t' = (dups', drops') in

    Logging.post_dump ppf "fuse" t' ;
    t'

  let rec specialize_drops shapes t =
    Logging.pre_dump ppf "drop_specialization" t;

    let rec optimize_disjoint dups drops =
      match drops with
      | [] -> (dups, [])
      | (_, y) :: drops ->
          let (y_dups, dups') =
            Ident.Set.partition (descendant shapes ~parent:y) dups
          in
          let (y_drops, drops') =
            List.partition ((fun (_, x) -> descendant shapes ~parent:y x)) drops
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
        let uniq = specialize_drops shapes (dups, children) in
        (DropInline { uniq; shared }, y)
    in

    let (fdups, fdrops) = fuse t in
    let t' = optimize_disjoint fdups fdrops in

    Logging.post_dump ppf "drop_specialization" t' ;
    t'

  let finalize ?(for_matched = false) shapes lam t =
    let rec helper lam (dups, drops) =
      let is_int x =
        let shape = Ident.Map.find_opt x shapes in
        match shape with
        | Some shape -> is_int shape
        | _ -> false
      in
      let merge_idups i1 i2 =
        Ident.Set.union i1 i2
      in
      let sequence_drop (op, x) (lam, idups) =
        match op with
        | DropRegular ->
            (Drop.sequence shapes x lam, idups)
        | DropInline { uniq ; shared } ->
            let idups0, shared = Ident.Set.partition (is_int) shared in
            let cond = Drop.is_unique x in
            let e1, idups1 = helper (Drop.free x) uniq in
            let e2 =
              Dup.sequence_many shapes shared @@
              Drop.decr x
            in
            let lam' = 
              Dup.sequence_many shapes idups @@
              Lsequence (Lifthenelse (cond, e1, e2), lam)
            in
            let idups' =
              merge_idups idups0 (merge_idups idups idups1)
            in
            (lam', idups')
      in
      let idups, rest_dups = Ident.Set.partition is_int dups in
      let drop_seq, idups' = List.fold_right sequence_drop drops (lam, idups) in
      let dup_seq = Dup.sequence_many shapes rest_dups in
      let lam' = dup_seq @@ drop_seq in
      (lam', idups')
    in
      (* The [Simplif] pass will transform every intvar to a field access. This
         is bad because it may lead to a use-after-free (for instance, when the
         parent was freed in the dup/drop preamble for a matched body

         To remedy this, we bind every intvar using [rc_dup_copy], which serves
         as a barrier that prevents [Simplif] from transforming intvars beyond
         this binding.
       *)
      let lam', int_dups = helper lam t in
      Dup.sequence_many ~bind_int:for_matched shapes int_dups @@ lam'
end
