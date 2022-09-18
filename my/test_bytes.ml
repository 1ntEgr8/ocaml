let () =
  let buf = Bytes.create 16 in
  Bytes.set buf 0 'a';
  Bytes.set buf 1 'b';
  Printf.printf "%c, %d, %d\n" (Bytes.get buf 0) 2 4
