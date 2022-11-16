open Obj

type color =
| Red
| Black;;

type node =
| Leaf
| Node of color * node * int * bool * node;;

let balance1 kv vv t n =
match n with
| Node (_, Node (Red, l, kx, vx, r1), ky, vy, r2) -> 
  let l = rc_dup_copy l in
  let r1 = rc_dup_copy r1 in 
  let r2 = rc_dup_copy r2 in 
  let kx = rc_copy kx in
  let vx = rc_copy vx in
  let ky = rc_copy ky in
  let vy = rc_copy vy in 
  rc_drop_ptr n;
  Node (Red, Node (Black, l, kx, vx, r1), ky, vy, Node (Black, r2, kv, vv, t))
| Node (_, l1, ky, vy, Node (Red, l2, kx, vx, r)) -> 
  let r = rc_dup_copy r in
  let l1 = rc_dup_copy l1 in 
  let l2 = rc_dup_copy l2 in 
  let kx = rc_copy kx in
  let vx = rc_copy vx in
  let ky = rc_copy ky in
  let vy = rc_copy vy in 
  rc_drop_ptr n;
  Node (Red, Node (Black, l1, ky, vy, l2), kx, vx, Node (Black, r, kv, vv, t))
| Node (_, l,  ky, vy, r) -> 
  let r = rc_dup_copy r in
  let l = rc_dup_copy l in 
  let ky = rc_copy ky in
  let vy = rc_copy vy in 
  rc_drop_ptr n;
  Node (Black, Node (Red, l, ky, vy, r), kv, vv, t)
| Leaf -> Leaf;;

let balance2 t kv vv n =
match n with
| Node (_, Node (Red, l, kx, vx, r1), ky, vy, r2)  -> 
  let l = rc_dup_copy l in
  let r1 = rc_dup_copy r1 in 
  let r2 = rc_dup_copy r2 in 
  let kx = rc_copy kx in
  let vx = rc_copy vx in
  let ky = rc_copy ky in
  let vy = rc_copy vy in 
  rc_drop_ptr n;
  Node (Red, Node (Black, t, kv, vv, l), kx, vx, Node (Black, r1, ky, vy, r2))
| Node (_, l1, ky, vy, Node (Red, l2, kx, vx, r)) -> 
  let r = rc_dup_copy r in
  let l1 = rc_dup_copy l1 in 
  let l2 = rc_dup_copy l2 in 
  let kx = rc_copy kx in
  let vx = rc_copy vx in
  let ky = rc_copy ky in
  let vy = rc_copy vy in 
  rc_drop_ptr n;
  Node (Red, Node (Black, t, kv, vv, l1), ky, vy, Node (Black, l2, kx, vx, r))
| Node (_, l, ky, vy, r) -> 
  let r = rc_copy r in
  let l = rc_dup_copy l in 
  let ky = rc_copy ky in
  let vy = rc_copy vy in 
  rc_drop_ptr n;
  Node (Black, t, kv, vv, Node (Red, l, ky, vy, r))
| Leaf -> Leaf;;

(* make borrowing *)
let is_red t =
match t with
| Node (Red, _, _, _, _) -> true
| _ -> false;;

let rec ins t kx vx =
match t with
| Leaf -> Node (Red, Leaf, kx, vx, Leaf)
| Node (Red, a, ky, vy, b) ->
  let a = rc_dup_copy a in
  let b = rc_dup_copy b in
  let ky = rc_copy ky in
  let vy = rc_copy vy in 
  rc_drop_ptr t; 
  if kx < ky then Node (Red, ins a kx vx, ky, vy, b)
  else if ky = kx then Node (Red, a, kx, vx, b)
  else Node (Red, a, ky, vy, ins b kx vx)
| Node (Black, a, ky, vy, b) ->
  let a = rc_dup_copy a in
  let b = rc_dup_copy b in
  let ky = rc_copy ky in
  let vy = rc_copy vy in 
  rc_drop_ptr t;
  if kx < ky then
    (if is_red a then balance1 ky vy b (ins a kx vx)
      else Node (Black, (ins a kx vx), ky, vy, b))
  else if kx = ky then Node (Black, a, kx, vx, b)
  else if is_red b then balance2 a ky vy (ins b kx vx)
       else Node (Black, a, ky, vy, (ins b kx vx));;

let set_black n =
match n with
| Node (_, l, k, v, r) -> let l = rc_dup_copy l in
                          let r = rc_dup_copy r in 
                          let k = rc_copy k in
                          let v = rc_copy v in   
                          rc_drop_ptr n;
                          Node (Black, l, k, v, r)
| e                    -> e;;

let insert t k v =
if is_red t then set_black (ins t k v)
else ins t k v;;

let rec fold f n d =
match n with
| Leaf -> d
| Node(_, l, k, v, r) -> 
  let r = rc_dup_copy r in
  let l = rc_dup_copy l in 
  (* let k = rc_dup_copy k in
  let v = rc_dup_copy v in *)
  rc_drop_ptr n;
  fold f r (f k v (fold f l d));;

let rec mk_map_aux n m =
if n = 0 then m
else let n1 = n-1 in
     mk_map_aux n1 (insert m n1 (n1 mod 10 == 0));;

let mk_map n = mk_map_aux n Leaf;;

let main n =
let t = mk_map n in
let v = fold (fun k v r -> if v then r + 1 else r) t 0 in
Printf.printf "%8d\n" v;
v;;

(* main (int_of_string Sys.argv.(1));; *)
main 4200000;; 
(* main 1362;; *)  (* any higher and segfault *) 

