let print_hello name =
  let msg = "Hello there!" in
  print_endline msg;
  let greet = "Welcome to OCaml, "^name in
  print_endline greet;
;;
