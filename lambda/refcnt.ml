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
end

module type Rc = sig
  include RcOp

  val sequence : shape Ident.Map.t -> Ident.t -> lambda -> lambda
  val sequence_many : shape Ident.Map.t -> Ident.Set.t -> lambda -> lambda
end

module MakeRc (R : RcOp) = struct
  include R

  let sequence shapes x expr =
    try
      let shape = Ident.Map.find x shapes in
      if is_int shape then
        expr
      else if is_ptr shape then
        Lsequence (ptr x, expr)
      else
        Lsequence (checked x, expr)
    with Not_found -> Lsequence (checked x, expr)

  let sequence_many shapes xs expr =
    Ident.Set.fold (sequence shapes) xs expr
end

module Dup = MakeRc (struct
  let ptr = prim dup_ptr_native_name
  let checked = prim dup_checked_native_name
end)

module Drop = struct
  include (MakeRc (struct
    let ptr = prim drop_ptr_native_name
    let checked = prim drop_checked_native_name
  end))
  
  let decr = prim decr_native_name
  let free = prim free_native_name
  let is_unique = prim is_unique_native_name
end
