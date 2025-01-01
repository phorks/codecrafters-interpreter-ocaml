let rec scan seq =
  match seq () with
  | Seq.Nil -> print_endline "EOF  null"
  | Seq.Cons (hd, tl) ->
      (match hd with
      | '(' -> print_endline "LEFT_PAREN ( null"
      | ')' -> print_endline "RIGHT_PAREN ) null"
      | '{' -> print_endline "LEFT_BRACE { null"
      | '}' -> print_endline "RIGHT_BRACE } null"
      | _ -> failwith "unexpected error");
      scan tl

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
