(* use refcounting; optimized with is_unique tests 
  > ./boot/ocamlrun ./ocamlopt -I ./stdlib -S -O2 -g -o deriv_rc2 my/deriv_rc2.ml   

  TODO: only converted add, the rest is still TODO
*)
open Obj;;

type expr =
| Val of int
| Var of string
| Add of expr * expr
| Mul of expr * expr
| Pow of expr * expr
| Ln of expr;;

let rec pown a n =
  if n == 0 then 1
  else if n == 1 then a
  else let b = pown a (n / 2) in
  b * b * (if n mod 2 == 0 then 1 else a);;

let rec add n m =
  match (n, m) with
  | (Val i, Val j) -> begin 
      let i = rc_copy i in
      let j = rc_copy j in
      rc_drop_ptr n;
      rc_drop_ptr m;
      Val (i+j)
    end
  | (Val 0, f)     -> begin rc_drop_ptr n; f end
  | (f, Val 0)     -> begin rc_drop_ptr m; f end
  | (f, Val i)     -> begin rc_drop_ptr m; add (Val i) f end
  | (Val i, Add((Val j) as v, f)) -> begin
       let i = rc_copy i in
       rc_drop_ptr n;
       let j = rc_copy j in
       let f = rc_copy f in
       rc_dup f;
       rc_drop_ptr m;
       add (Val (i+j)) f
    end
  | (f, Add((Val i) as v, g)) -> begin
      let i = rc_copy i in
      let g = rc_copy g in
      rc_dup g;
      rc_drop_ptr m;
      add (Val i) (add f g)
    end
  | (Add(f, g), h)         -> begin
      let f = rc_copy f in
      let g = rc_copy g in
      rc_drop n;
      add f (add g h)
    end
  | (f, g)  -> Add (f, g);;

let rec mul n m =
match (n, m) with
| (Val n, Val m) -> Val (n*m)
| (Val 0, _)     -> Val 0
| (_, Val 0)     -> Val 0
| (Val 1, f)     -> f
| (f, Val 1)     -> f
| (f, Val n)     -> mul (Val n) f
| (Val n, Mul (Val m, f)) -> mul (Val (n*m)) f
| (f, Mul (Val n, g))     -> mul (Val n) (mul f g)
| (Mul (f, g), h)         -> mul f (mul g h)
| (f, g)                  -> Mul (f, g);;

let rec pow m n =
match (m, n) with
| (Val m, Val n) -> Val (pown m n)
| (_,  Val 0)    -> Val 1
| (f, Val 1)     -> f
| (Val 0, _)     -> Val 0
| (f, g)         -> Pow (f, g);;

let rec ln n =
match n with
| (Val 1) -> Val 0
| f       -> Ln f;;

let rec d x f =
match f with
| Val _      -> Val 0
| Var y      -> if x = y then Val 1 else Val 0
| Add (f, g) -> add (d x f) (d x g)
| Mul (f, g) -> add (mul f (d x g)) (mul g (d x f))
| Pow (f, g) -> mul (pow f g) (add (mul (mul g (d x f)) (pow f (Val (-1)))) (mul (ln f) (d x g)))
| Ln f       -> mul (d x f) (pow f (Val (-1)));;

let rec count f =
match f with
| Val _ -> 1
| Var _ -> 1
| Add (f, g) -> count f + count g
| Mul (f, g) -> count f + count g
| Pow (f, g) -> count f + count g
| Ln f       -> count f;;

let rec nest_aux s f n x =
if n == 0 then x
else let x = f (s - n) x in
     nest_aux s f (n - 1) x;;

let nest f n e =
nest_aux n f n e;;

let deriv i f =
  let d = d "x" f in
  Printf.printf "%8d count: %8d\n" (i+1) (count d);
  d;;

let x = Var "x" in
let f = pow x x in
nest deriv 10 f;;
