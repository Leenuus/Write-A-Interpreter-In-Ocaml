open Interpreter.Lexer

let input = "let add = fn(x, y) {\nx + y;\n}; "
let l = Option.get (new_lexer input)
let l, t = next_token l
let () = assert (Let = t)
let l, t = next_token l
let () = assert (Ident "add" = t)
let l, t = next_token l
let () = assert (Assign = t)
let l, t = next_token l
let () = assert (Funct = t)
let l, t = next_token l
let () = assert (Lparen = t)
let l, t = next_token l
let () = assert (Ident "x" = t)
let l, t = next_token l
let () = assert (Comma = t)
let l, t = next_token l
let () = assert (Ident "y" = t)
let l, t = next_token l
let () = assert (Rparen = t)
let l, t = next_token l
let () = assert (Lbrace = t)
(* line break *)
let l, t = next_token l
let () = assert (Ident "x" = t)
let l, t = next_token l
let () = assert (Plus = t)
let l, t = next_token l
let () = assert (Ident "y" = t)
let l, t = next_token l
let () = assert (Semicolon = t)
(* line break *)
(* };  *)
let l, t = next_token l
let () = assert (Rbrace = t)
let l, t = next_token l
let () = assert (Semicolon = t)
let l, t = next_token l
let () = assert (Eof = t)
let _ = l


let input = "let result = add(five, ten);"
let l = Option.get (new_lexer input)
let l, t = next_token l
let () = assert (Let = t)
let l, t = next_token l
let () = assert (Ident "result" = t)
let l, t = next_token l
let () = assert (Assign = t)
let l, t = next_token l
let () = assert (Ident "add" = t)
let l, t = next_token l
let () = assert (Lparen = t)
let l, t = next_token l
let () = assert (Ident "five" = t)
let l, t = next_token l
let () = assert (Comma = t)
let l, t = next_token l
let () = assert (Ident "ten" = t)
let l, t = next_token l
let () = assert (Rparen = t)
let l, t = next_token l
let () = assert (Semicolon = t)
let l, t = next_token l
let () = assert (Eof = t)
let _ = l

let input = "!-/*5;"
let l = Option.get (new_lexer input)
let l, t = next_token l
let () = assert (Bang = t)
let l, t = next_token l
let () = assert (Dash = t)
let l, t = next_token l
let () = assert (Slash = t)
let l, t = next_token l
let () = assert (Asterisk = t)
let l, t = next_token l
let () = assert (LiteralInt 5 = t)
let l, t = next_token l
let () = assert (Semicolon = t)
let l, t = next_token l
let () = assert (Eof = t)
let _ = l

let input = "5 < 10 > 5;"
let l = Option.get (new_lexer input)
let l, t = next_token l
let () = assert (LiteralInt 5 = t)
let l, t = next_token l
let () = assert (Lthan = t)
let l, t = next_token l
let () = assert (LiteralInt 10 = t)
let l, t = next_token l
let () = assert (Gthan = t)
let l, t = next_token l
let () = assert (LiteralInt 5 = t)
let l, t = next_token l
let () = assert (Semicolon = t)
let l, t = next_token l
let () = assert (Eof = t)
let _ = l