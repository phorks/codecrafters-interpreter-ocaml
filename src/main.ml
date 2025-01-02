open Scanner

let tokenize file_contents =
  let rec aux (tokens : token_result List.t) =
    match tokens with
    | List.[] -> ()
    | List.(hd :: tl) ->
        (match hd with
        | Error err ->
            Printf.eprintf "[line %d] Error: %s\n" err.line err.msg;
            ()
        | Ok token ->
            Printf.printf "%s %s %s\n" (tt_string token.tt) token.lexeme
              (tt_literal token.tt);
            ());
        aux tl
  in
  let scanner = Scanner.scanner file_contents in
  let tokens, result = Scanner.scan scanner in
  aux tokens;
  result

let rec map_tokens tokens =
  match tokens with
  | List.[] -> Some List.[]
  | List.(hd :: tl) -> (
      match map_tokens tl with
      | Some tl' -> (
          match hd with Ok token -> Some List.(token :: tl') | _ -> None)
      | None -> None)

let parse file_contents =
  let scanner = Scanner.scanner file_contents in
  let tokens, _ = Scanner.scan scanner in
  match map_tokens tokens with
  | Some tokens -> (
      let x = Parser.parse_expr ((List.to_seq tokens) ()) in
      match x with
      | Ok (expr, _) ->
          Printf.printf "%s\n" (Parser.pretty_print expr);
          true
      | Error err ->
          let line, at_msg =
            match err with
            | Some token -> (token.line, token_error_at_msg token)
            | None -> (-1, "end")
          in
          Printf.eprintf "[line %d] Error at %s: Expect expression.\n" line
            at_msg;
          false)
  | None -> false

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "Usage: ./your_program.sh tokenize <filename>\n";
    exit 1);

  let command = Sys.argv.(1) in

  let filename = Sys.argv.(2) in

  match command with
  | "tokenize" -> (
      let file_contents =
        In_channel.with_open_text filename In_channel.input_all
      in
      let result = tokenize file_contents in
      match result with Successful -> exit 0 | HadError -> exit 65)
  | "parse" ->
      let file_contents =
        In_channel.with_open_text filename In_channel.input_all
      in
      let result = parse file_contents in
      if result then exit 0 else exit 1
  | _ ->
      Printf.eprintf "Unknown command: %s\n" command;
      exit 1

(* let has_error = scan (String.to_seq file_contents) 1 in *)
(* if has_error then exit 65 *)

(* (* You can use print statements as follows for debugging, they'll be visible when running tests. *) *)
(* (* Printf.eprintf "Logs from your program will appear here!\n"; *) *)
(* if String.length file_contents > 0 then *)
(*   (* Implement & use your scanner here *) *)
(*   failwith "Scanner not implemented" *)
(* else *)
(*   (* Uncomment this block to pass the first stage *) *)
(*   print_endline "EOF  null"; *)
(* (* Placeholder, remove this line when implementing the scanner *) *)
(* () *)
