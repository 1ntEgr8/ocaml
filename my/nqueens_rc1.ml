(* use refcounting; optimized with _ptr tests when possible 
  > ./boot/ocamlrun ./ocamlopt -I ./stdlib -S -O2 -g -o nqueens_rc1 my/nqueens_rc1.ml     
*)
open Obj;;
open List;;

(* assume xs is borrowed *)
let rec safe queen diag xs = 
  match xs with
  | q :: qs -> queen <> q && queen <> q + diag && queen <> q - diag && safe queen (diag + 1) qs
  | [] -> true;;

let rec append_safe queen xs xss =
  if (queen <= 0) then begin 
    rc_drop xs;
    xss
  end
  else if (safe queen 1 xs) then begin
    rc_dup xs;
    append_safe (queen - 1) xs ((queen :: xs) :: xss)
  end
  else append_safe (queen - 1) xs xss;;

let rec extend queen acc xss =
  match xss with
  | xs :: rest -> begin 
      let xs = rc_copy xs in
      let rest = rc_copy rest in
      rc_dup xs;
      rc_dup rest;
      rc_drop_ptr xss;
      extend queen (append_safe queen xs acc) rest
    end
  | [] -> acc;;


let rec find_solutions n queen =
  if (queen == 0) then [[]]
   else extend n [] (find_solutions n (queen - 1));;

let rec len_acc xs n =
  match xs with
  | _ :: xx -> begin
      let xx = rc_copy xx in
      rc_dup xx;
      rc_drop_ptr xs;
      len_acc xx (n+1)
    end
  | [] -> n;;

let len xs = len_acc xs 0

   
let queens n = len (find_solutions n n);;

Printf.printf "%8d\n" (queens 13);;
