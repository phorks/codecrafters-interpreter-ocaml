open Scanner

let ( let+ ) = Result.bind

type interpreter_error = IESyntaxError | IERuntimeError | IEUnknownCommand

let interpreter_error_code e =
  match e with
  | IESyntaxError -> 65
  | IERuntimeError -> 70
  | IEUnknownCommand -> 1

let tokenize_cmd file_contents =
  let rec aux (tokens : token_result List.t) =
    match tokens with
    | List.[] -> ()
    | List.(hd :: tl) ->
        (match hd with
        | Error err -> Printf.eprintf "%s" (lexical_error_to_string err)
        | Ok token ->
            Printf.printf "%s %s %s\n" (tt_string token.tt) token.lexeme
              (tt_literal token.tt);
            ());
        aux tl
  in
  let scanner = Scanner.scanner file_contents in
  let tokens, result = Scanner.scan scanner in
  aux tokens;
  match result with Successful -> Ok () | HadError -> Error IESyntaxError

let rec filter_lexical_errors tokens =
  match tokens with
  | List.[] -> List.[]
  | List.(hd :: tl) -> (
      let rest = filter_lexical_errors tl in
      match hd with Ok _ -> rest | Error e -> List.(e :: rest))

let rec map_tokens tokens =
  match tokens with
  | List.[] -> Ok List.[]
  | List.(hd :: tl) -> (
      match hd with
      | Ok token -> (
          match map_tokens tl with
          | Ok tl' -> Ok List.(token :: tl')
          | Error e -> Error e)
      | Error e ->
          let tl' = filter_lexical_errors tl in
          Error List.(e :: tl'))

let tokenize tokens =
  match map_tokens tokens with
  | Ok tokens -> Ok tokens
  | Error lexical_errors ->
      let _ =
        List.map
          (fun e -> Printf.eprintf "%s" (lexical_error_to_string e))
          lexical_errors
      in
      Error IESyntaxError

let parse tokens =
  let+ tokens = tokenize tokens in
  match Parser.parse_expr (List.to_seq tokens) with
  | Ok (expr, _) -> Ok expr
  | Error err ->
      Printf.eprintf "%s" (Parser.syntax_error_to_string err);
      Error IESyntaxError

let parse_cmd file_contents =
  let scanner = Scanner.scanner file_contents in
  let tokens, _ = Scanner.scan scanner in
  let+ expr = parse tokens in
  Printf.printf "%s\n" (Parser.pretty_print expr);
  Ok ()

let evaluate file_contents =
  let scanner = Scanner.scanner file_contents in
  let tokens, _ = Scanner.scan scanner in
  let+ expr = parse tokens in
  let env = Evaluation.Environment.empty in
  match Evaluation.eval expr env with
  | Ok (v, _) ->
      Printf.printf "%s\n" (Evaluation.pretty_print v);
      Ok ()
  | Error err ->
      Printf.eprintf "%s" (Evaluation.runtime_error_to_string err);
      Error IERuntimeError

let parse_stmts tokens =
  let+ tokens = tokenize tokens in
  match Statements.parse (List.to_seq tokens) with
  | Ok stmts -> Ok stmts
  | Error err ->
      Printf.eprintf "%s" (Parser.syntax_error_to_string err);
      Error IESyntaxError

let exec_cmd file_contents =
  let scanner = Scanner.scanner file_contents in
  let tokens, _ = Scanner.scan scanner in
  let+ stmts = parse_stmts tokens in
  let env = Evaluation.Environment.empty in
  match Execution.exec stmts env with
  | Ok _ -> Ok ()
  | Error err ->
      Printf.eprintf "%s" (Evaluation.runtime_error_to_string err);
      Error IERuntimeError

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "Usage: ./your_program.sh tokenize <filename>\n";
    exit 1);

  let command = Sys.argv.(1) in

  let filename = Sys.argv.(2) in

  let result =
    match command with
    | "tokenize" ->
        let file_contents =
          In_channel.with_open_text filename In_channel.input_all
        in
        tokenize_cmd file_contents
    | "parse" ->
        let file_contents =
          In_channel.with_open_text filename In_channel.input_all
        in
        parse_cmd file_contents
    | "evaluate" ->
        let file_contents =
          In_channel.with_open_text filename In_channel.input_all
        in
        evaluate file_contents
    | "run" ->
        let file_contents =
          In_channel.with_open_text filename In_channel.input_all
        in
        exec_cmd file_contents
    | _ ->
        Printf.eprintf "Unknown command: %s\n" command;
        Error IEUnknownCommand
  in

  match result with
  | Ok _ -> exit 0
  | Error e -> exit (interpreter_error_code e)
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
