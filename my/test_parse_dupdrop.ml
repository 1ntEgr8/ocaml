let () =
  let x = [1;2;3] in
  Printf.printf "init: %d\n" (Obj.get_refcount x) ;
  my_dup x ;
  Printf.printf "after dup: %d\n" (Obj.get_refcount x) ;
  my_dup x ;
  Printf.printf "after dup: %d\n" (Obj.get_refcount x) ;
  my_dup x ;
  Printf.printf "after dup: %d\n" (Obj.get_refcount x) ;
  my_drop x ;
  Printf.printf "after drop: %d\n" (Obj.get_refcount x) ;
  my_drop x ;
  Printf.printf "after drop: %d\n" (Obj.get_refcount x) ;
  my_drop x ;
  Printf.printf "after drop: %d\n" (Obj.get_refcount x) ;
