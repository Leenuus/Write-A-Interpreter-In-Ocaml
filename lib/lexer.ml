open Base

(* we use enum to present our tokens *)
type token =
  | Ilegal (* Not implemented or crazy input *)
  (* delimiter1 *)
  | Semicolon (* DONE: *)
  | Comma (* DONE: *)
  | Eof (* DONE: *)
  (* delimiter2 *)
  | Lparen (* DONE: *)
  | Rparen (* DONE: *)
  | Lbrace (* DONE: *)
  | Rbrace (* DONE: *)
  | Lbracket (* DONE: *)
  | Rbracket (* DONE: *)
  (* keyword *)
  | Assign (* DONE: *)
  | Let (* DONE: *)
  | Funct (* DONE: *)
  | True (* DONE: *)
  | False (* DONE: *)
  | If (* DONE: *)
  | Else (* DONE: *)
  | Return (* DONE: *)
  (* misc *)
  | Plus (* DONE: *)
  | Dash (* DONE: *)
  | Asterisk (* DONE: *)
  | Bang (* DONE: *)
  | Slash (* DONE: *)
  | Percent (* DONE: *)
  | Lthan (* DONE: *)
  | Gthan (* DONE: *)
  (* two chars *)
  | Eq
  | NotEq
  (* indentifier *)
  | Ident of string (* DONE: *)
  (* Literal *)
  | LiteralInt of int (* DONE: *)
  (* TODO: Float Literal  *)
  | LiteralFloat of float
  (* TODO: quotes and literal string  *)
  | Dquote
  | Squote
  | LiteralStr of string
[@@deriving show]

(* NOTE:we are going to write a parse token function *)
(*  let's name it as `next_token` *)

(* NOTE: this lexer records  *)
(*         1. the input string *)
(*         2. the position we are reading now *)
(*         3. the character in the position we are reading *)

type lexer = { input : string; pos : int; ch : char option }

(* NOTE: lexer contructor *)
let new_lexer (input : string) =
  match input with
  "" -> None
  | _ -> Some { input; pos = 0; ch = Some (String.get input 0) }

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

(* NOTE: what if we meet EOF when we are parsing a identifier? *)
(* here I think it means the identitfier we are parsing is ended, so we just return it *)

let lookup_ident (s : string) =
  match s with
  | "let" -> Let
  | "fn" -> Funct
  | "if" -> If
  | "else" -> Else
  | "true" -> True
  | "false" -> False
  | "return" -> Return
  | _ -> Ident s

(* NOTE: don't pass an empty string to this function
   (* as acc, use the __leading character__ as acc *) *)
let rec next_ident (l : lexer) (acc : string) =
  match l.ch with
  | None -> (l, lookup_ident acc)
  | Some c ->
      if Char.is_alpha c || Char.is_digit c then
        next_ident (next_char l) (acc ^ String.make 1 c)
      else if is_delimeter c then (l, lookup_ident acc)
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

let peek_char (l : lexer) =
  let next = l.pos + 1 in
  if next >= String.length l.input then None else Some (String.get l.input next)

let deal_with_eq_sign (l : lexer) =
  match peek_char l with
  | None -> (next_char l, Assign)
  | Some c ->
      if Char.( = ) c '=' then (next_char (next_char l), Eq)
      else (next_char l, Assign)

let deal_with_bang (l : lexer) =
  match peek_char l with
  | None -> (next_char l, Bang)
  | Some c ->
      if Char.( = ) c '=' then (next_char (next_char l), NotEq)
      else (next_char l, Bang)

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
  | Some '=' -> deal_with_eq_sign l
  | Some '+' -> (next_char l, Plus)
  | Some '-' -> (next_char l, Dash)
  | Some '*' -> (next_char l, Asterisk)
  | Some '/' -> (next_char l, Slash)
  | Some '%' -> (next_char l, Percent)
  | Some '!' -> deal_with_bang l
  | Some '>' -> (next_char l, Gthan)
  | Some '<' -> (next_char l, Lthan)
  (* | Some '\'' ->   *)
  | Some c ->
      (* NOTE: deal with indentifier or keyword which  *)
      (* always starts with `[a-zA-Z]` *)
      if Char.is_alpha c then next_ident (next_char l) (String.make 1 c)
      else if Char.is_digit c then next_num (next_char l) (String.make 1 c)
      else (l, Ilegal)
;;
