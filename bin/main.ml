open Interpreter.Lexer

let quit = ref false;;

let deal_with_line () =
  let () = print_endline "Please enter a monkey expression" in
  (* TODO: deal with end of file input *)
  let input = read_line () in
  let l = new_lexer input in
  match l with
  | None -> true
  | Some l -> let tokens = all_tokens l [] in
    let res = List.map (fun t -> show_token t) tokens in
    let () = print_endline  (String.concat " " res) in
    false
  ;;

while not !quit do
  quit := deal_with_line ()
done;;