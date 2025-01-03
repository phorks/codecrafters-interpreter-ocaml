type statement =
  | STExpression of Parser.exp
  | STPrint of Parser.exp
  | STVarDecl of string * Parser.exp option

let ( let* ) = Option.bind
let ( let+ ) = Result.bind

let expect_tt (seq : Scanner.token Seq.node) err_msg f =
  match seq with
  | Seq.Nil -> Error (Parser.SyntaxError (None, err_msg))
  | Seq.Cons (hd, tl) -> (
      match f hd.tt with
      | Some x -> Ok (x, tl)
      | None -> Error (Parser.SyntaxError (Some hd, err_msg)))

let expect_semicolon seq after_what =
  let+ _, rest =
    expect_tt seq (Printf.sprintf "Expect ';' after %s" after_what) (function
      | Scanner.Semicolon -> Some ()
      | _ -> None)
  in
  Ok rest

let expect_ident seq err_msg =
  expect_tt seq err_msg (function Scanner.Ident name -> Some name | _ -> None)

let expect_tt_opt (seq : Scanner.token Seq.node) f =
  match seq with
  | Seq.Nil -> None
  | Seq.Cons (hd, tl) -> Option.map (fun x -> (x, tl)) (f hd.tt)

let expect_equal_opt seq =
  expect_tt_opt seq (function Scanner.Equal -> Some () | _ -> None)

let parse_expr seq =
  let+ expr, rest = Parser.parse_expr seq in
  let+ rest = expect_semicolon rest "expression" in
  Ok (STExpression expr, rest)

let parse_print seq =
  let+ expr, rest = Parser.parse_expr seq in
  let+ rest = expect_semicolon rest "value" in
  Ok (STPrint expr, rest)

let parse_var_decl seq =
  let+ name, rest = expect_ident seq "Expect variable name" in
  let+ init, rest =
    match expect_equal_opt (rest ()) with
    | Some (_, tl) -> (
        match Parser.parse_expr (tl ()) with
        | Ok (expr, tl) -> Ok (Some expr, tl)
        | Error err -> Error err)
    | None -> Ok (None, rest ())
  in
  let+ rest = expect_semicolon rest "variable declaration" in
  Ok (STVarDecl (name, init), rest)

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
            | Scanner.Reserved Scanner.VarKeyword -> parse_var_decl (tl ())
            | _ -> parse_expr (seq ())
          in
          let+ rest = parse rest in
          Ok List.(stmt :: rest))
