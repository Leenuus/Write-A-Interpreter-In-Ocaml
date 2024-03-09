open Interpreter.Lexer

(* parse if and return *)
let input = "if (5 < 10) {\nreturn true;\n} else {\nreturn false;\n}"
let l = new_lexer input
let l, t = next_token l
let () = assert (t = If)
let l, t = next_token l
let () = assert (t = Lparen)
let l, t = next_token l
let () = assert (t = LiteralInt 5)
let l, t = next_token l
let () = assert (t = Lthan)
let l, t = next_token l
let () = assert (t = LiteralInt 10)
let l, t = next_token l
let () = assert (t = Rparen)
let l, t = next_token l
let () = assert (t = Lbrace)

(* line break *)
(* return true; *)
let l, t = next_token l
let () = assert (t = Return)
let l, t = next_token l
let () = assert (t = True)
let l, t = next_token l
let () = assert (t = Semicolon)

(* line break *)
(* } else { *)
let l, t = next_token l
let () = assert (t = Rbrace)
let l, t = next_token l
let () = assert (t = Else)
let l, t = next_token l
let () = assert (t = Lbrace)

(* line break *)
(* return false; *)
let l, t = next_token l
let () = assert (t = Return)
let l, t = next_token l
let () = assert (t = False)
let l, t = next_token l
let () = assert (t = Semicolon)

(* line break *)
let l, t = next_token l
let () = assert (t = Rbrace)
let _ = l