let input1 = "=+(){},;"
let l = Interpreter.Lexer.new_lexer input1
let l, t = Interpreter.Lexer.next_token l
let () = assert (Interpreter.Lexer.Assign = t)
let l, t = Interpreter.Lexer.next_token l
let () = assert (Interpreter.Lexer.Plus = t)
let l, t = Interpreter.Lexer.next_token l
let () = assert (Interpreter.Lexer.Lparen = t)
let l, t = Interpreter.Lexer.next_token l
let () = assert (Interpreter.Lexer.Rparen = t)
let l, t = Interpreter.Lexer.next_token l
let () = assert (Interpreter.Lexer.Lbrace = t)
let l, t = Interpreter.Lexer.next_token l
let () = assert (Interpreter.Lexer.Rbrace = t)
let l, t = Interpreter.Lexer.next_token l
let () = assert (Interpreter.Lexer.Comma = t)
let l, t = Interpreter.Lexer.next_token l
let () = assert (Interpreter.Lexer.Semicolon = t)
let _, t = Interpreter.Lexer.next_token l
let () = assert (Interpreter.Lexer.Eof = t)

let input2 = "let a = 1;"
let l = Interpreter.Lexer.new_lexer input2
let l, t = Interpreter.Lexer.next_token l
let () = assert (Interpreter.Lexer.Let = t)
let l, t = Interpreter.Lexer.next_token l
let () = assert (Interpreter.Lexer.Ident "a" = t)
let l, t = Interpreter.Lexer.next_token l
let () = assert (Interpreter.Lexer.Assign = t)
let l, t = Interpreter.Lexer.next_token l
let () = assert (Interpreter.Lexer.LiteralInt 1 = t)
let l, t = Interpreter.Lexer.next_token l
let () = assert (Interpreter.Lexer.Semicolon = t)
let l, t = Interpreter.Lexer.next_token l
let () = assert (Interpreter.Lexer.Eof = t)

(* NOTE: haha, a simple trick to get rid of those complains on `not used var` *)
let _ = l