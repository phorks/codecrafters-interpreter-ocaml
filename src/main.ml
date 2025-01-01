let report_lexical_error line msg =
  Printf.eprintf "[line %d] Error: %s\n" line msg

let match_head seq ch =
  match seq () with
  | Seq.Nil -> (false, seq)
  | Seq.Cons (hd, tl) -> if hd == ch then (true, tl) else (false, seq)

let match_string_literal seq =
  let rec f seq literal =
    match seq () with
    | Seq.Nil -> (None, seq)
    | Seq.Cons (hd, tl) -> (
        if hd == '"' then (Some literal, tl)
        else
          match f tl (literal ^ Char.escaped hd) with
          | None, tl' -> (None, tl')
          | Some s, tl' -> (Some s, tl'))
  in
  f seq String.empty

let rec scan seq line =
  match seq () with
  | Seq.Nil ->
      print_endline "EOF  null";
      false
  | Seq.Cons (hd, tl) ->
      let line', has_error, rest =
        match hd with
        | '(' ->
            print_endline "LEFT_PAREN ( null";
            (line, false, tl)
        | ')' ->
            print_endline "RIGHT_PAREN ) null";
            (line, false, tl)
        | '{' ->
            print_endline "LEFT_BRACE { null";
            (line, false, tl)
        | '}' ->
            print_endline "RIGHT_BRACE } null";
            (line, false, tl)
        | ',' ->
            print_endline "COMMA , null";
            (line, false, tl)
        | '.' ->
            print_endline "DOT . null";
            (line, false, tl)
        | '-' ->
            print_endline "MINUS - null";
            (line, false, tl)
        | '+' ->
            print_endline "PLUS + null";
            (line, false, tl)
        | ';' ->
            print_endline "SEMICOLON ; null";
            (line, false, tl)
        | '*' ->
            print_endline "STAR * null";
            (line, false, tl)
        | '!' ->
            let next_eq, tl' = match_head tl '=' in
            print_endline
              (if next_eq then "BANG_EQUAL != null" else "BANG ! null");
            (line, false, tl')
        | '=' ->
            let next_eq, tl' = match_head tl '=' in
            print_endline
              (if next_eq then "EQUAL_EQUAL == null" else "EQUAL = null");
            (line, false, tl')
        | '<' ->
            let next_eq, tl' = match_head tl '=' in
            print_endline
              (if next_eq then "LESS_EQUAL <= null" else "LESS < null");
            (line, false, tl')
        | '>' ->
            let next_eq, tl' = match_head tl '=' in
            print_endline
              (if next_eq then "GREATER_EQUAL >= null" else "GREATER > null");
            (line, false, tl')
        | '/' ->
            let next_slash, tl' = match_head tl '/' in
            let next_line, tl'' =
              if next_slash then
                ( line + 1,
                  Seq.drop 1 (Seq.drop_while (fun ch -> ch <> '\n') tl') )
              else (
                print_endline "SLASH / null";
                (line, tl'))
            in
            (next_line, false, tl'')
        | '"' ->
            let literal, tl' = match_string_literal tl in
            (match literal with
            | None -> report_lexical_error line "Unterminated string."
            | Some literal ->
                Printf.printf "STRING \"%s\" %s" literal literal;
                ());
            (line, false, tl')
        | ' ' | '\r' | '\t' -> (line, false, tl)
        | '\n' -> (line + 1, false, tl)
        | _ ->
            Printf.eprintf "[line %d] Error: Unexpected character: %c\n" line hd;
            (line, true, tl)
      in
      scan rest line' || has_error

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

  let has_error = scan (String.to_seq file_contents) 1 in
  if has_error then exit 65

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
