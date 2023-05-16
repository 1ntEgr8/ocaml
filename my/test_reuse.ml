open Obj
open List

let rec map xs f =
  match xs with
  | x' :: xx' ->
      let x  = rc_copy x' in
      let xx = rc_copy xx' in
      let ru = if (rc_ptr_is_unique xs) then rc_ptr_reuse xs else begin rc_dup x; rc_dup xx; rc_ptr_decr_null xs end in
      let y  = (rc_ptr_dup_copy f) x in
      rc_reuse_at ru (y :: (map xx f))
  | [] ->
      rc_drop xs;
      rc_drop f;
      []

let rec sum_acc xs acc =
  match xs with
  | x :: xx ->
      let x = rc_copy x in 
      let xx = rc_copy xx in
      (* rc_dup x; *)
      if (rc_ptr_is_unique xs) then rc_ptr_free xs else begin (* rc_dup x; *) rc_dup xx; rc_ptr_decr xs end;
      sum_acc xx (x + acc)
  | [] -> 
      rc_drop xs;
      acc

let sum xs = sum_acc xs 0


let () =
  let xs = init 10 (fun x -> x) in
  let ys = map xs (fun x -> x + 1) in
  let s = sum ys in
  Printf.printf "sum %i (should be 55)\n" s
  
