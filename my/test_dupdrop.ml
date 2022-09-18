let rec map xs f =
  match xs with
  | x :: xx ->
      my_dup xx ;
      my_drop xs ;
      my_dup f ;
      (f x) :: (map xx f)
  | [] ->
      my_drop xs ;
      my_drop f ;
      []

let () =
  let a = [22; 23; 24; 25] in
  let _b = map a (fun x -> x + 1) in
  ()
