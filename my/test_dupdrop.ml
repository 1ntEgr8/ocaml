let rec map xs f =
  match xs with
  | x :: xx ->
      my_dup x;
      my_dup xx;
      my_drop xs;
      my_dup f;
      (f x) :: (map xx f)
  | [] ->
      my_drop xs;
      my_drop f;
      []

let rec sum_acc xs acc =
  match xs with
  | x :: xx ->
      (* my_dup x; *)
      my_dup xx;
      my_drop xs;
      sum_acc xx (acc + 1)
  | [] -> 
      my_drop xs;
      acc

let sum xs = sum_acc xs 0

let () =
  let xs = [22; 23; 24; 25] in
  let ys = map xs (fun x -> x + 1) in
  let s = sum ys in
  Printf.printf "sum %i\n" s
  