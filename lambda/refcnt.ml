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
  type drops = Ident.Set.t

  type op =
    | RcDup of Ident.t
    | RcDrop of Ident.t

  let dup x = RcDup x
  let drop x = RcDrop x

  let fuse (dups, drops) =
    (Ident.Set.diff dups drops, Ident.Set.diff drops dups)

  let combine (dups, drops) =
    (List.map dup (Ident.Set.elements dups)) @
    (List.map drop (Ident.Set.elements drops))

  let specialize_drops shapes t =
    (* optimize : (dups * drops) -> op list *)
    let rec optimize t =
      let (fdups, fdrops) = fuse t in
      optimize_disjoint (fdups, Ident.Set.elements fdrops)

    (* optimize_disjoint : (dups * dropsl) -> op list *)
    and optimize_disjoint (dups, drops) =
      match drops with
        | [] -> List.map dup (Ident.Set.elements dups)
        | y :: drops -> (
          let (ydups, dups') = Ident.Set.partition (descendant shapes y) dups in
          let (ydrops, drops') = List.partition (descendant shapes y) drops in
          let rest = optimize_disjoint (dups', drops') in
          let prefix = List.map drop ydrops in
          let spec = specialize_drop ydups y in
          rest @ prefix @ spec
        )

    (* specialize_drop : dups -> ident -> op list *)
    and specialize_drop _ydups _y = []

    in 
      optimize t

  let finalize shapes lam ops =
    List.fold_right (fun op lam ->
      match op with
      | RcDup x -> Dup.sequence ~bind_int:true shapes x lam
      | RcDrop x -> Drop.sequence ~bind_int: true shapes x lam
    ) ops lam
end
