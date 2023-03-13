type list =
  | Cons of int * list
  | Nil

let rec map f xs =
  match xs with
  | Cons (x, rest) -> Cons (f x, map f rest)
  | Nil -> Nil

let rec iter f xs =
  match xs with
  | Cons (x, rest) -> f x ; iter f rest
  | Nil -> ()

let () =
  let xs = Cons (1, Cons (2, Cons (3, Nil))) in
  let ys = map (fun x -> x + 1) xs in
  iter (fun x -> print_int x) ys ;
  print_newline () ;
  print_string "expected: ";
  print_int 234 ;
