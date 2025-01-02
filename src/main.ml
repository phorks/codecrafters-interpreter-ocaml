open Scanner

let tokenize file_contents =
  let rec aux (tokens : token_result List.t) =
    match tokens with
    | List.[] -> ()
    | List.(hd :: tl) -> (
        match hd with
        | Error err ->
            Printf.eprintf "[line %d] Error: %s\n" err.line err.msg;
            ()
        | Ok token ->
            Printf.printf "%s %s %s\n" (tt_string token.tt) token.lexeme
              (tt_literal token.tt);
            aux tl;
            ())
  in
  let scanner = Scanner.scanner file_contents in
  let tokens, result = Scanner.scan scanner in
  aux tokens;
  result

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "Usage: ./your_program.sh tokenize <filename>\n";
    exit 1);

  let command = Sys.argv.(1) in

  let filename = Sys.argv.(2) in

  if command <> "tokenize" then (
    Printf.eprintf "Unknown command: %s\n" command;
    exit 1);

  let file_contents = In_channel.with_open_text filename In_channel.input_all in
  let result = tokenize file_contents in
  match result with Successful -> exit 0 | HadError -> exit 65

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
