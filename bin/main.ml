open Interpreter.Lexer


(* TODO: write a repl *)
(* learn how to write ocaml loop *)
let input = read_line ()
let l = new_lexer input
let () =
  match l with
  None -> exit 1
  | Some l ->
  let _, t = next_token l in
  let () = print_endline (show_token t) in
  exit 0