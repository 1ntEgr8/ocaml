type 'a list = 
  | Cons of 'a * 'a list
  | Nil

let rec map f xs =
  match xs with
  | Cons (x, rest) -> Cons (f x, map f rest)
  | Nil -> Nil

let () =
  let xs = Cons (1, Cons (2, Cons (3, Nil))) in
  let _ys = map (fun x -> x + 1) xs in
  ()
