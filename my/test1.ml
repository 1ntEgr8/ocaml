let rec iterate r x_init i = 
  if i = 1 then x_init 
  else 
    let x = iterate r x_init (i - 1) in 
    r *. x *. (1.0 -. x) 

let () = 
  Random.self_init (); 
  for x = 0 to 100 do 
    let r = 4.0 *. float_of_int x /. 640.0 in 
    for i = 0 to 39 do 
      let x_init = Random.float 1.0 in 
      let x_final = iterate r x_init 500 in 
      let y = int_of_float (x_final *. 480.) in 
      () 
    done 
  done; 
  Gc.print_stat stdout 
