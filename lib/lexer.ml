open Base

(* we use enum to present our tokens *)
type token =
  | (* delimiter1 *)
    Semicolon
  (* Not implemented or crazy input *)
  | Ilegal
  | Comma
  | Eof
  (* delimiter2 *)
  | Lparen
  | Rparen
  | Lbrace
  | Rbrace
  | Lbracket
  | Rbracket
  (* keyword *)
  | Assign
  | Let
  | Funcc
  (* alrithmetic *)
  | Plus
  | Minus
  | Mul
  | Div
  | Mod
  (* indentifier *)
  | Ident of string
  (* Literal *)
  | LiteralInt of int
  | LiteralFloat of int
  (* TODO: quotes  *)
  | Dquote
  | Squote
  | LiteralStr of int

(* NOTE:we are going to write a parse token function *)
(*  let's name it as `next_token` *)

(* NOTE: this lexer records  *)
(*         1. the input string *)
(*         2. the position we are reading now *)
(*         3. the character in the position we are reading *)

type lexer = { input : string; pos : int; ch : char option }

(* NOTE: lexer contructor *)
let new_lexer (input : string) =
  { input; pos = 0; ch = Some (String.get input 0) }

let next_char (l : lexer) =
  {
    input = l.input;
    pos = l.pos + 1;
    ch =
      (if String.length l.input - 1 > l.pos then
         Some (String.get l.input (l.pos + 1))
       else None);
  }

let is_delimeter (c : char) =
  match c with
  | '[' -> true
  | ']' -> true
  | '(' -> true
  | ')' -> true
  | '{' -> true
  | '}' -> true
  | ';' -> true
  | ',' -> true
  | '=' -> true
  | '+' -> true
  | '-' -> true
  | '*' -> true
  | '/' -> true
  | '%' -> true
  | ' ' -> true
  | _ -> false

(* TODO: what if we meet EOF when we are parsing a identifier? *)
(* here I think it means the identitfier we are parsing is ended, so we just return it *)

let keyword_or_ident (s: string) = 
  match s with
  "let" -> Let
  | "function" -> Funcc
  | _ -> Ident s

(* NOTE: don't parse an empty string as acc, use the __leading character__ as acc *)
let rec next_ascii (l : lexer) (acc : string) =
  match l.ch with
  | None -> (l, keyword_or_ident acc)
  | Some c ->
      if Char.is_alpha c || Char.is_digit c then next_ascii (next_char l) (acc ^ String.make 1 c)
      else if is_delimeter c then (l, keyword_or_ident acc)
      else (l, Ilegal)

let rec next_num (l : lexer) (acc : string) =
  match l.ch with
  | None -> (l, LiteralInt (Int.of_string acc))
  | Some c ->
      if Char.is_digit c then next_num (next_char l) (acc ^ String.make 1 c)
      else if is_delimeter c then (l, LiteralInt (Int.of_string acc))
      else (l, Ilegal)

let skip_whitespace (l : lexer) =
  match l.ch with
  | None -> l
  | Some c -> if Char.is_whitespace c then next_char l else l

(* TODO: write a return value annotation *)
let rec next_token (l : lexer) =
  let l = skip_whitespace l in
  match l.ch with
  (* NOTE: skip whitespaces or meet the end of input *)
  | None -> (l, Eof)
  | Some ' ' -> next_token l
  (* NOTE: delimeters *)
  | Some '[' -> (next_char l, Lbracket)
  | Some ']' -> (next_char l, Rbracket)
  | Some '(' -> (next_char l, Lparen)
  | Some ')' -> (next_char l, Rparen)
  | Some '{' -> (next_char l, Lbrace)
  | Some '}' -> (next_char l, Rbrace)
  | Some ';' -> (next_char l, Semicolon)
  | Some ',' -> (next_char l, Comma)
  | Some '=' -> (next_char l, Assign)
  (* NOTE: alrithmetic *)
  | Some '+' -> (next_char l, Plus)
  | Some '-' -> (next_char l, Minus)
  | Some '*' -> (next_char l, Mul)
  | Some '/' -> (next_char l, Div)
  | Some '%' -> (next_char l, Mod)
  | Some c ->
      (* NOTE: deal with indentifier or keyword which  *)
      (* always starts with `[a-zA-Z]` *)
      if Char.is_alpha c then next_ascii (next_char l) (String.make 1 c)
      else if Char.is_digit c then next_num (next_char l) (String.make 1 c)
      else (l, Ilegal)