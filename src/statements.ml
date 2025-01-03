type statement = STExpression of Parser.exp | STPrint of Parser.exp

let ( let* ) = Option.bind
let ( let+ ) = Result.bind

let expect_semicolon (seq : Scanner.token Seq.node) after_what =
  let err_msg = Printf.sprintf "Expect ';' after %s." after_what in
  match seq with
  | Seq.Nil -> Error (Parser.SyntaxError (None, err_msg))
  | Seq.Cons (hd, tl) -> (
      match hd.tt with
      | Scanner.Semicolon -> Ok tl
      | _ -> Error (Parser.SyntaxError (Some hd, err_msg)))

let parse_print seq =
  let+ expr, rest = Parser.parse_expr seq in
  let+ rest = expect_semicolon rest "value" in
  Ok (STPrint expr, rest)

let parse_expr seq =
  let+ expr, rest = Parser.parse_expr seq in
  let+ rest = expect_semicolon rest "expression" in
  Ok (STExpression expr, rest)

let rec parse (seq : Scanner.token Seq.t) =
  match seq () with
  | Seq.Nil -> Ok List.[]
  | Seq.Cons (hd, tl) -> (
      match hd.tt with
      | Scanner.Eof -> Ok List.[]
      | _ ->
          let+ stmt, rest =
            match hd.tt with
            | Scanner.Reserved Scanner.PrintKeyword -> parse_print (tl ())
            | _ -> parse_expr (seq ())
          in
          let+ rest = parse rest in
          Ok List.(stmt :: rest))
