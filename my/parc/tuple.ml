let dup x = (x, x)

let () =
  let _x = dup [] in
  ()
