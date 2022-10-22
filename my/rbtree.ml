type color =
| Red
| Black;;

type node =
| Leaf
| Node of color * node * int * bool * node;;

let balance1 kv vv t n =
match n with
| Node (_, Node (Red, l, kx, vx, r1), ky, vy, r2) -> 
  let l = my_dup l in
  let r1 = my_dup r1 in 
  let r2 = my_dup r2 in 
  let kx = my_dup kx in
  let vx = my_dup vx in
  let ky = my_dup ky in
  let vy = my_dup vy in
  my_drop n;
  Node (Red, Node (Black, l, kx, vx, r1), ky, vy, Node (Black, r2, kv, vv, t))
| Node (_, l1, ky, vy, Node (Red, l2, kx, vx, r)) -> 
  let r = my_dup r in
  let l1 = my_dup l1 in 
  let l2 = my_dup l2 in 
  let kx = my_dup kx in
  let vx = my_dup vx in
  let ky = my_dup ky in
  let vy = my_dup vy in
  my_drop n;
  Node (Red, Node (Black, l1, ky, vy, l2), kx, vx, Node (Black, r, kv, vv, t))
| Node (_, l,  ky, vy, r) -> 
  let r = my_dup r in
  let l = my_dup l in 
  let ky = my_dup ky in
  let vy = my_dup vy in
  my_drop n;
  Node (Black, Node (Red, l, ky, vy, r), kv, vv, t)
| Leaf -> Leaf;;

let balance2 t kv vv n =
match n with
| Node (_, Node (Red, l, kx, vx, r1), ky, vy, r2)  -> 
  let l = my_dup l in
  let r1 = my_dup r1 in 
  let r2 = my_dup r2 in 
  let kx = my_dup kx in
  let vx = my_dup vx in
  let ky = my_dup ky in
  let vy = my_dup vy in
  my_drop n;
  Node (Red, Node (Black, t, kv, vv, l), kx, vx, Node (Black, r1, ky, vy, r2))
| Node (_, l1, ky, vy, Node (Red, l2, kx, vx, r)) -> 
  let r = my_dup r in
  let l1 = my_dup l1 in 
  let l2 = my_dup l2 in 
  let kx = my_dup kx in
  let vx = my_dup vx in
  let ky = my_dup ky in
  let vy = my_dup vy in
  my_drop n;
  Node (Red, Node (Black, t, kv, vv, l1), ky, vy, Node (Black, l2, kx, vx, r))
| Node (_, l, ky, vy, r) -> 
  let r = my_dup r in
  let l = my_dup l in 
  let ky = my_dup ky in
  let vy = my_dup vy in
  my_drop n;
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
  let a = my_dup a in
  let b = my_dup b in
  let ky = my_dup ky in
  let vy = my_dup vy in
  my_drop t; 
  if kx < ky then Node (Red, ins a kx vx, ky, vy, b)
  else if ky = kx then Node (Red, a, kx, vx, b)
  else Node (Red, a, ky, vy, ins b kx vx)
| Node (Black, a, ky, vy, b) ->
  let a = my_dup a in
  let b = my_dup b in
  let ky = my_dup ky in
  let vy = my_dup vy in
  my_drop t;
  if kx < ky then
    (if is_red a then balance1 ky vy b (ins a kx vx)
      else Node (Black, (ins a kx vx), ky, vy, b))
  else if kx = ky then Node (Black, a, kx, vx, b)
  else if is_red b then balance2 a ky vy (ins b kx vx)
       else Node (Black, a, ky, vy, (ins b kx vx));;

let set_black n =
match n with
| Node (_, l, k, v, r) -> let l = my_dup l in
                          let r = my_dup r in 
                          let k = my_dup k in
                          let v = my_dup v in  
                          my_drop n;
                          Node (Black, l, k, v, r)
| e                    -> e;;

let insert t k v =
if is_red t then set_black (ins t k v)
else ins t k v;;

let rec fold f n d =
match n with
| Leaf -> d
| Node(_, l, k, v, r) -> 
  let r = my_dup r in
  let l = my_dup l in 
  let k = my_dup k in
  let v = my_dup v in  
  my_drop n;
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

