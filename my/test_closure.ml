let test () =
  let n = Random.int 10 in
  
  let rec f x y = n + n + y + g x
  and g y = f y (y + 1) in

  f 1 2
