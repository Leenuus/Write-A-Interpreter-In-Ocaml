open Interpreter.Lexer

(* t1 *)
let input = "let a = 1;"
let l = new_lexer input
let l, t = next_token l
let () = assert (Let = t)
let l, t = next_token l
let () = assert (Ident "a" = t)
let l, t = next_token l
let () = assert (Assign = t)
let l, t = next_token l
let () = assert (LiteralInt 1 = t)
let l, t = next_token l
let () = assert (Semicolon = t)
let l, t = next_token l
let () = assert (Eof = t)
let _ = l

(* t2 *)
let input = "let five = 5;"
let l = new_lexer input
let l, t = next_token l
let () = assert (Let = t)
let l, t = next_token l
let () = assert (Ident "five" = t)
let l, t = next_token l
let () = assert (Assign = t)
let l, t = next_token l
let () = assert (LiteralInt 5 = t)
let l, t = next_token l
let () = assert (Semicolon = t)
let l, t = next_token l
let () = assert (Eof = t)
let _ = l

(* t3 *)
let input = "let ten = 10;"
let l = new_lexer input
let l, t = next_token l
let () = assert (Let = t)
let l, t = next_token l
let () = assert (Ident "ten" = t)
let l, t = next_token l
let () = assert (Assign = t)
let l, t = next_token l
let () = assert (LiteralInt 10 = t)
let l, t = next_token l
let () = assert (Semicolon = t)
let l, t = next_token l
let () = assert (Eof = t)
let _ = l