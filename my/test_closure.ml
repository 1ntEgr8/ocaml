let test () =
  let n = Random.int 10 in
  let n' = ref 0 in
  let xs = List.init n (fun i -> i) in

  let rec f x y = 
    n' := !n' + 1;
    if !n' > 10 then
      List.length xs 
    else
      n + n + y + g x

  and g y =
    n' := !n' + 3;
    f y (y + 1)

  in

  Printf.printf "%d\n" (f 5 1)
;;

test ()
