let rec rng a b =
  if a = b then
    []
  else
    a :: (rng (a+1) b)

let () =
  Random.self_init ();
  let xs = Array.of_list (rng 1 5) in
  let sum = Array.fold_left (fun acc el -> acc + el) 0 xs in
  Printf.printf "%d\n" sum
