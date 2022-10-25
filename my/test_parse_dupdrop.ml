open Obj

let () =
  let n = Random.int 10 in
  let x = List.init n (fun i -> i) in
  Printf.printf "init: %d\n" (Obj.get_refcount x) ;
  let x = my_dup x in
  Printf.printf "after dup: %d\n" (Obj.get_refcount x) ;
  let x = my_dup x in
  Printf.printf "after dup: %d\n" (Obj.get_refcount x) ;
  let x = my_dup x in
  Printf.printf "after dup: %d\n" (Obj.get_refcount x) ;
  my_drop x ;
  Printf.printf "after drop: %d\n" (Obj.get_refcount x) ;
  my_drop x ;
  Printf.printf "after drop: %d\n" (Obj.get_refcount x) ;
  my_drop x ;
  Printf.printf "after drop: %d\n" (Obj.get_refcount x) ;
