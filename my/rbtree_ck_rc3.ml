(* use refcounting; optimized with is_unique tests *)
(* > ./boot/ocamlrun ./ocamlopt -I ./stdlib -S -O2 -g -o rbtree_rc2 my/rbtree_rc2.ml
*)
open Obj

type color =
| Red
| Black;;

type node =
| Leaf
| Node of color * node * int * bool * node;;

let balance1 ru kv vv t n =
match n with
| Node (_, (Node (Red, l, kx, vx, r1) as nl), ky, vy, r2) -> 
  let nl = rc_copy nl in
  let l  = rc_copy l in
  let r1 = rc_copy r1 in 
  let r2 = rc_copy r2 in 
  let kx = rc_copy kx in
  let vx = rc_copy vx in
  let ky = rc_copy ky in
  let vy = rc_copy vy in 
  let ru_n = if (rc_ptr_is_unique n)  then rc_ptr_reuse n else begin rc_dup nl; rc_dup r2; rc_ptr_decr_null n end in
  let ru_nl = if (rc_ptr_is_unique nl) then rc_ptr_reuse nl else begin rc_dup l; rc_dup r1; rc_ptr_decr_null nl end in
  (* rc_ptr_drop n; *)
  rc_reuse_at ru_n (Node (Red, rc_reuse_at ru_nl (Node (Black, l, kx, vx, r1)), ky, vy, rc_reuse_at ru (Node (Black, r2, kv, vv, t))))
| Node (_, l1, ky, vy, (Node (Red, l2, kx, vx, r) as nr)) -> 
  let r  = rc_copy r in
  let nr = rc_copy nr in
  let l1 = rc_copy l1 in 
  let l2 = rc_copy l2 in 
  let kx = rc_copy kx in
  let vx = rc_copy vx in
  let ky = rc_copy ky in
  let vy = rc_copy vy in 
  let ru_n = if (rc_ptr_is_unique n)  then rc_ptr_reuse n else begin rc_dup l1; rc_dup nr; rc_ptr_decr_null n end in
  let ru_nr = if (rc_ptr_is_unique nr) then rc_ptr_reuse nr else begin rc_dup l2; rc_dup r; rc_ptr_decr_null nr end in
  (* rc_ptr_drop n; *)
  rc_reuse_at ru_nr (Node (Red, rc_reuse_at ru_n (Node (Black, l1, ky, vy, l2)), kx, vx, rc_reuse_at ru (Node (Black, r, kv, vv, t))))
| Node (_, l,  ky, vy, r) -> 
  let r  = rc_copy r in
  let l  = rc_copy l in 
  let ky = rc_copy ky in
  let vy = rc_copy vy in 
  let ru_n = if (rc_ptr_is_unique n) then rc_ptr_reuse n else begin rc_dup r; rc_dup l; rc_ptr_decr_null n end in
  (* rc_ptr_drop n; *)
  rc_reuse_at ru (Node (Black, rc_reuse_at ru_n (Node (Red, l, ky, vy, r)), kv, vv, t))
| Leaf -> 
    rc_reuse_drop ru;
    Leaf;;

let balance2 ru t kv vv n =
match n with
| Node (_, (Node (Red, l, kx, vx, r1) as nl), ky, vy, r2)  -> 
  let nl = rc_copy nl in
  let l  = rc_copy l in
  let r1 = rc_copy r1 in 
  let r2 = rc_copy r2 in 
  let kx = rc_copy kx in
  let vx = rc_copy vx in
  let ky = rc_copy ky in
  let vy = rc_copy vy in 
  let ru_n  = if (rc_ptr_is_unique n) then rc_ptr_reuse n else begin rc_dup nl; rc_dup r2; rc_ptr_decr_null n end  in
  let ru_nl = if (rc_ptr_is_unique nl) then rc_ptr_reuse nl else begin rc_dup l; rc_dup r1; rc_ptr_decr_null nl end in
  rc_reuse_at ru_nl (Node(Red, rc_reuse_at ru (Node (Black, t, kv, vv, l)), kx, vx, rc_reuse_at ru_n (Node (Black, r1, ky, vy, r2))))

| Node (_, l1, ky, vy, (Node (Red, l2, kx, vx, r) as nr)) -> 
  let r  = rc_copy r in
  let nr = rc_copy nr in
  let l1 = rc_copy l1 in 
  let l2 = rc_copy l2 in 
  let kx = rc_copy kx in
  let vx = rc_copy vx in
  let ky = rc_copy ky in
  let vy = rc_copy vy in 
  let ru_n = if (rc_ptr_is_unique n)  then rc_ptr_reuse n else begin rc_dup l1; rc_dup nr; rc_ptr_decr_null n end in
  let ru_nr = if (rc_ptr_is_unique nr) then rc_ptr_reuse nr else begin rc_dup l2; rc_dup r; rc_ptr_decr_null nr end in
  (* rc_ptr_drop n; *)
  rc_reuse_at ru_n (Node (Red, rc_reuse_at ru (Node (Black, t, kv, vv, l1)), ky, vy, rc_reuse_at ru_nr (Node (Black, l2, kx, vx, r))))

| Node (_, l, ky, vy, r) -> 
  let r  = rc_copy r in
  let l  = rc_copy l in 
  let ky = rc_copy ky in
  let vy = rc_copy vy in 
  let ru_n = if (rc_ptr_is_unique n) then rc_ptr_reuse n else begin rc_dup r; rc_dup l; rc_ptr_decr_null n end in
  (* rc_ptr_drop n; *)
  rc_reuse_at ru (Node (Black, t, kv, vv, rc_reuse_at ru_n (Node (Red, l, ky, vy, r))))

| Leaf -> 
    rc_reuse_drop ru;
    Leaf;;  

(* make borrowing *)
let is_red t =
match t with
| Node (Red, _, _, _, _) -> true
| _ -> false;;

let rec ins t kx vx =
match t with
| Leaf -> Node (Red, Leaf, kx, vx, Leaf)
| Node (Red, a, ky, vy, b) ->
  let a = rc_copy a in
  let b = rc_copy b in
  let ky = rc_copy ky in
  let vy = rc_copy vy in 
  let ru = if (rc_ptr_is_unique t) then rc_ptr_reuse t 
                               else begin rc_dup a; rc_dup b; rc_ptr_decr_null t end in
  if kx < ky then rc_reuse_at ru (Node (Red, ins a kx vx, ky, vy, b))
  else if ky = kx then rc_reuse_at ru (Node (Red, a, kx, vx, b))
  else rc_reuse_at ru (Node (Red, a, ky, vy, ins b kx vx))
| Node (Black, a, ky, vy, b) ->
  let a = rc_copy a in
  let b = rc_copy b in
  let ky = rc_copy ky in
  let vy = rc_copy vy in 
  let ru = if (rc_ptr_is_unique t) then rc_ptr_reuse t 
                               else begin rc_dup a; rc_dup b; rc_ptr_decr_null t end in
  if kx < ky then
    (if is_red a then balance1 ru ky vy b (ins a kx vx)
      else rc_reuse_at ru (Node (Black, (ins a kx vx), ky, vy, b)))
  else if kx = ky then rc_reuse_at ru (Node (Black, a, kx, vx, b))
  else if is_red b then balance2 ru a ky vy (ins b kx vx)
       else rc_reuse_at ru (Node (Black, a, ky, vy, (ins b kx vx)));;

let set_black n =
match n with
| Node (_, l, k, v, r) -> let l = rc_copy l in
                          let r = rc_copy r in 
                          let k = rc_copy k in
                          let v = rc_copy v in   
                          let ru = if (rc_ptr_is_unique n) then rc_ptr_reuse n 
                                                       else begin rc_dup l; rc_dup r; rc_ptr_decr_null n end in
                          rc_reuse_at ru (Node (Black, l, k, v, r))
| e                    -> e;;

let insert t k v =
if is_red t then set_black (ins t k v)
else ins t k v;;

let rec fold f n d =
match n with
| Leaf -> d
| Node(_, l, k, v, r) -> 
  let r = rc_copy r in
  let l = rc_copy l in 
  let k = rc_copy k in
  let v = rc_copy v in 
  if (rc_ptr_is_unique n) then rc_ptr_free n
                      else begin rc_dup l; rc_dup r; rc_ptr_decr n end;
  fold f r (f k v (fold f l d));;

  let rec mk_map_aux freq n m acc =
    if n = 0 then m::acc
    else let m1 = insert m n (n mod 10 == 0)
         in mk_map_aux freq (n-1) m1 (if ((n mod freq) == 0) then begin rc_dup m1; m1::acc end else acc);;
    
  let mk_map freq n = mk_map_aux freq n Leaf [];;
  
  
  
  let rec drop_list xs =
    match xs with
    | x :: xx -> begin
       let x = rc_copy x in
       let xx = rc_copy xx in     
       if (rc_ptr_is_unique xs) then begin rc_drop x; rc_ptr_free xs end else begin rc_dup xx; rc_ptr_decr xs end;
       drop_list xx
      end
    | [] -> ()
  
  
  let head xs =
    match xs with
    | x :: xx -> begin
       let x = rc_copy x in
       let xx = rc_copy xx in     
       if (rc_ptr_is_unique xs) then begin drop_list xx; rc_ptr_free xs end else begin rc_dup x; rc_ptr_decr xs end;
       x
      end;;
  
  let main freq n =
  let ms = mk_map freq n in
  let v = fold (fun k v r -> if v then r + 1 else r) (head ms) 0 in
  Printf.printf "%8d\n" v;
  v;;
  
  (* main (int_of_string Sys.argv.(1));; *)
  main 5 4200000;;
  