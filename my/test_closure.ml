open Obj

let test () =
  let n = Random.int 10 in
  let n' = ref 0 in
  let xs = List.init n (fun i -> i) in

  let rec f x y = 
    n' := !n' + 1;
    if !n' > 10 then
      List.length xs 
    else begin
      let g = rc_dup_copy g in
      n + n + y + g x
    end

  and g y =
    n' := !n' + 3;
    let f = rc_dup_copy f in
    f y (y + 1)

  in

  Printf.printf "%d\n" (f 5 1)
;;

test ()
