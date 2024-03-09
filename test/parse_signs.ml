open Interpreter.Lexer

let input1 = "=+(){},;"
let l = new_lexer input1
let l, t = next_token l
let () = assert (Assign = t)
let l, t = next_token l
let () = assert (Plus = t)
let l, t = next_token l
let () = assert (Lparen = t)
let l, t = next_token l
let () = assert (Rparen = t)
let l, t = next_token l
let () = assert (Lbrace = t)
let l, t = next_token l
let () = assert (Rbrace = t)
let l, t = next_token l
let () = assert (Comma = t)
let l, t = next_token l
let () = assert (Semicolon = t)
let _, t = next_token l
let () = assert (Eof = t)

(* test eq  *)
let input = "10 == 10;"
let l = new_lexer input
let l, t = next_token l
let () = assert (LiteralInt 10 = t)
let l, t = next_token l
let () = assert (Eq = t)
let l, t = next_token l
let () = assert (LiteralInt 10 = t)
let _ = l

(* test not eq  *)
let input = "10 != 9;"
let l = new_lexer input
let l, t = next_token l
let () = assert (LiteralInt 10 = t)
let l, t = next_token l
let () = assert (NotEq = t)
let l, t = next_token l
let () = assert (LiteralInt 9 = t)
let _ = l