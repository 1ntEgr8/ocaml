open List

let rec map xs f =
  match xs with
  | x' :: xx' ->
      let x = my_dup x' in
      let xx = my_dup xx' in
      my_drop xs;
      (* my_dup f; *)
      (f x) :: (map xx f)
  | [] ->
      (* my_drop xs; *)
      (* my_drop f; *)
      []

let rec sum_acc xs acc =
  match xs with
  | x' :: xx' ->
      let x = my_dup x' in
      let xx = my_dup xx' in
      my_drop xs;  (* x is not cached ! *)
      sum_acc xx (x + acc)
  | [] -> 
      my_drop xs;
      acc

let sum xs = sum_acc xs 0

let () =
  let xs = init 10 (fun x -> x) in
  let ys = map xs (fun x -> x + 1) in
  let s = sum ys in
  Printf.printf "sum %i (should be 55)\n" s
  