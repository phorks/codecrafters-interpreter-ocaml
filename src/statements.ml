type statement =
  | STExpr of Parser.exp
  | STPrint of Parser.exp
  | STVarDecl of string * Parser.exp option
  | STBlock of statement list
  | STIf of Parser.exp * statement * statement option
  | STWhile of Parser.exp * statement

let ( let* ) = Option.bind
let ( let+ ) = Result.bind

let expect_tt (seq : Scanner.token Seq.t) err_msg f =
  match seq () with
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

let expect_left_paren seq after_what =
  let+ _, rest =
    expect_tt seq (Printf.sprintf "Expect '(' after %s" after_what) (function
      | Scanner.LeftParen -> Some ()
      | _ -> None)
  in
  Ok rest

let expect_right_paren seq after_what =
  let+ _, rest =
    expect_tt seq (Printf.sprintf "Expect ')' after %s" after_what) (function
      | Scanner.RightParen -> Some ()
      | _ -> None)
  in
  Ok rest

let expect_ident seq err_msg =
  expect_tt seq err_msg (function Scanner.Ident name -> Some name | _ -> None)

let expect_tt_opt (seq : Scanner.token Seq.t) f =
  match seq () with
  | Seq.Nil -> None
  | Seq.Cons (hd, tl) -> Option.map (fun x -> (x, tl)) (f hd.tt)

let expect_equal_opt seq =
  expect_tt_opt seq (function Scanner.Equal -> Some () | _ -> None)

let parse_expr seq =
  let+ expr, rest = Parser.parse_expr seq in
  let+ rest = expect_semicolon rest "expression" in
  Ok (STExpr expr, rest)

let parse_print seq =
  let+ expr, rest = Parser.parse_expr seq in
  let+ rest = expect_semicolon rest "value" in
  Ok (STPrint expr, rest)

let parse_var_decl seq =
  let+ name, rest = expect_ident seq "Expect variable name" in
  let+ init, rest =
    match expect_equal_opt rest with
    | Some (_, tl) -> (
        match Parser.parse_expr tl with
        | Ok (expr, tl) -> Ok (Some expr, tl)
        | Error err -> Error err)
    | None -> Ok (None, rest)
  in
  let+ rest = expect_semicolon rest "variable declaration" in
  Ok (STVarDecl (name, init), rest)

let rec parse (seq : Scanner.token Seq.t) =
  match seq () with
  | Seq.Nil | Seq.Cons ({ tt = Scanner.Eof; _ }, _) -> Ok List.[]
  | _ ->
      let+ stmt, rest = parse_single seq in
      let+ rest = parse rest in
      Ok List.(stmt :: rest)

and parse_single (seq : Scanner.token Seq.t) =
  match seq () with
  | Seq.Nil -> parse_expr Seq.empty
  | Seq.Cons (hd, tl) -> (
      match hd.tt with
      | Scanner.Reserved Scanner.PrintKeyword -> parse_print tl
      | Scanner.Reserved Scanner.VarKeyword -> parse_var_decl tl
      | Scanner.LeftBrace -> parse_block tl
      | Scanner.Reserved Scanner.IfKeyword -> parse_if tl
      | Scanner.Reserved Scanner.WhileKeyword -> parse_while tl
      | Scanner.Reserved Scanner.ForKeyword -> parse_for tl
      | _ -> parse_expr seq)

and parse_block (seq : Scanner.token Seq.t) =
  let rec aux (seq : Scanner.token Seq.t) =
    match seq () with
    | Seq.Nil -> Ok (List.[], seq)
    | Seq.Cons (({ tt = Scanner.Eof; _ } as hd), _) ->
        Error (Parser.SyntaxError (Some hd, "Expect '}' after block"))
    | Seq.Cons ({ tt = Scanner.RightBrace; _ }, tl) -> Ok (List.[], tl)
    | _ ->
        let+ stmt, rest = parse_single seq in
        let+ stmts, rest = aux rest in
        Ok (List.(stmt :: stmts), rest)
  in
  let+ stmts, rest = aux seq in
  Ok (STBlock stmts, rest)

and parse_if (seq : Scanner.token Seq.t) =
  let+ rest = expect_left_paren seq "'if'" in
  let+ expr, rest = Parser.parse_expr rest in
  let+ rest = expect_right_paren rest "if condition" in
  let+ body, rest = parse_single rest in
  let+ else_branch, rest =
    match
      expect_tt_opt rest (fun tt ->
          match tt with
          | Scanner.Reserved Scanner.ElseKeyword -> Some ()
          | _ -> None)
    with
    | Some (_, rest) ->
        let+ else_branch, rest = parse_single rest in
        Ok (Some else_branch, rest)
    | _ -> Ok (None, rest)
  in
  Ok (STIf (expr, body, else_branch), rest)

and parse_while (seq : Scanner.token Seq.t) =
  let+ rest = expect_left_paren seq "'while'" in
  let+ expr, rest = Parser.parse_expr rest in
  let+ rest = expect_right_paren rest "condition" in
  let+ body, rest = parse_single rest in
  Ok (STWhile (expr, body), rest)

and parse_for (seq : Scanner.token Seq.t) =
  let+ rest = expect_left_paren seq "'for'" in
  let+ init, rest =
    match rest () with
    | Seq.Cons ({ tt = Scanner.Semicolon; _ }, tl) -> Ok (None, tl)
    | Seq.Cons ({ tt = Scanner.Reserved Scanner.VarKeyword; _ }, tl) ->
        let+ init, rest = parse_var_decl tl in
        Ok (Some init, rest)
    | _ ->
        let+ init, rest = parse_expr rest in
        Ok (Some init, rest)
  in
  let+ cond, rest =
    match rest () with
    | Seq.Cons (({ tt = Scanner.Semicolon; _ } as hd), tl) ->
        Ok
          (Parser.Literal (Parser.ExpToken.exp_token hd (Parser.LBool true)), tl)
    | _ ->
        let+ cond, rest = Parser.parse_expr rest in
        Ok (cond, rest)
  in
  let+ rest = expect_semicolon rest "loop condition" in
  let+ step, rest =
    match rest () with
    | Seq.Cons ({ tt = Scanner.RightParen; _ }, _) ->
        Ok
          ( None,
            rest
            (* the right parenthesis
               shouldn't be skipped *) )
    | _ ->
        let+ step, rest = Parser.parse_expr rest in
        Ok (Some step, rest)
  in
  let+ rest = expect_right_paren rest "clauses" in
  let+ body, rest = parse_single rest in
  let body =
    match step with
    | Some step_stmt -> STBlock List.(body :: [ STExpr step_stmt ])
    | _ -> body
  in
  let while_stmt = STWhile (cond, body) in
  let stmt =
    match init with
    | Some init_stmt -> STBlock List.(init_stmt :: [ while_stmt ])
    | _ -> while_stmt
  in
  Ok (stmt, rest)
