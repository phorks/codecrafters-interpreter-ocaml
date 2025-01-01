let rec scan seq line =
  match seq () with
  | Seq.Nil -> print_endline "EOF  null"
  | Seq.Cons (hd, tl) ->
      let line' =
        match hd with
        | '(' ->
            print_endline "LEFT_PAREN ( null";
            line
        | ')' ->
            print_endline "RIGHT_PAREN ) null";
            line
        | '{' ->
            print_endline "LEFT_BRACE { null";
            line
        | '}' ->
            print_endline "RIGHT_BRACE } null";
            line
        | ',' ->
            print_endline "COMMA , null";
            line
        | '.' ->
            print_endline "DOT . null";
            line
        | '-' ->
            print_endline "MINUS - null";
            line
        | '+' ->
            print_endline "PLUS + null";
            line
        | ';' ->
            print_endline "SEMICOLON ; null";
            line
        | '*' ->
            print_endline "STAR * null";
            line
        | '\n' -> line + 1
        | _ ->
            failwith
              (Printf.sprintf "[line %d] Error: Unexpected character: %c" line
                 hd)
      in
      scan tl line'

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

  scan (String.to_seq file_contents)

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
