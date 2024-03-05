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

let input2 = "let a = 1;"
let l = new_lexer input2
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

let input3 = "let five = 5;
let ten = 10;
let add = fn(x, y) {
x + y;
};
let result = add(five, ten);
!-/*5;
5 < 10 > 5;"

(* let five = 5; *)
let l = new_lexer input3
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

(* let ten = 10; *)
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

(* let add = fn(x, y) { *)
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

(* x + y; *)
let l, t = next_token l
let () = assert (Ident "x" = t)
let l, t = next_token l
let () = assert (Plus = t)
let l, t = next_token l
let () = assert (Ident "y" = t)
let l, t = next_token l
let () = assert (Semicolon = t)

(* }; *)
let l, t = next_token l
let () = assert (Rbrace = t)
let l, t = next_token l
let () = assert (Semicolon = t)

(* NOTE: haha, a simple trick to get rid of those complains on `not used var` *)
let _ = l