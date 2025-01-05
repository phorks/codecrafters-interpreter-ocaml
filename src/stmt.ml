type t =
  | STExpr of Expr.t
  | STPrint of Expr.t
  | STVarDecl of string * Expr.t option
  | STBlock of t list
  | STIf of Expr.t * t * t option
  | STWhile of Expr.t * t
  | STFn of string * string list * t
  | STRet of Expr.t

let ( let* ) = Option.bind
let ( let+ ) = Result.bind

open Parsing

let parse_expr seq =
  let+ expr, rest = Expr.parse seq in
  let+ rest = expect_semicolon rest "expression" in
  Ok (STExpr expr, rest)

let parse_print seq =
  let+ expr, rest = Expr.parse seq in
  let+ rest = expect_semicolon rest "value" in
  Ok (STPrint expr, rest)

let parse_var_decl seq =
  let+ name, rest = expect_ident seq "Expect variable name" in
  let+ init, rest =
    match expect_equal_opt rest with
    | Some tl -> (
        match Expr.parse tl with
        | Ok (expr, tl) -> Ok (Some expr, tl)
        | Error err -> Error err)
    | None -> Ok (None, rest)
  in
  let+ rest = expect_semicolon rest "variable declaration" in
  Ok (STVarDecl (name, init), rest)

let rec parse (seq : Token.t Seq.t) =
  match seq () with
  | Seq.Nil | Seq.Cons ({ tt = Token.Eof; _ }, _) -> Ok List.[]
  | _ ->
      let+ stmt, rest = parse_single seq in
      let+ rest = parse rest in
      Ok List.(stmt :: rest)

and parse_single (seq : Token.t Seq.t) =
  match seq () with
  | Seq.Nil -> parse_expr Seq.empty
  | Seq.Cons (hd, tl) -> (
      match hd.tt with
      | Token.Reserved Token.PrintKeyword -> parse_print tl
      | Token.Reserved Token.VarKeyword -> parse_var_decl tl
      | Token.LeftBrace -> parse_block tl
      | Token.Reserved Token.IfKeyword -> parse_if tl
      | Token.Reserved Token.WhileKeyword -> parse_while tl
      | Token.Reserved Token.ForKeyword -> parse_for tl
      | Token.Reserved Token.FunKeyword -> parse_fun tl "function"
      | Token.Reserved Token.ReturnKeyword -> parse_return tl
      | _ -> parse_expr seq)

and parse_block (seq : Token.t Seq.t) =
  let rec aux (seq : Token.t Seq.t) =
    match seq () with
    | Seq.Nil -> Ok (List.[], seq)
    | Seq.Cons (({ tt = Token.Eof; _ } as hd), _) ->
        Error (Err.SyntaxError (Some hd, "Expect '}' after block"))
    | Seq.Cons ({ tt = Token.RightBrace; _ }, tl) -> Ok (List.[], tl)
    | _ ->
        let+ stmt, rest = parse_single seq in
        let+ stmts, rest = aux rest in
        Ok (List.(stmt :: stmts), rest)
  in
  let+ stmts, rest = aux seq in
  Ok (STBlock stmts, rest)

and parse_if (seq : Token.t Seq.t) =
  let+ rest = expect_left_paren seq "'if'" in
  let+ expr, rest = Expr.parse rest in
  let+ rest = expect_right_paren rest "if condition" in
  let+ body, rest = parse_single rest in
  let+ else_branch, rest =
    match
      expect_tt_opt rest (function
        | Token.Reserved Token.ElseKeyword -> true
        | _ -> false)
    with
    | Some rest ->
        let+ else_branch, rest = parse_single rest in
        Ok (Some else_branch, rest)
    | _ -> Ok (None, rest)
  in
  Ok (STIf (expr, body, else_branch), rest)

and parse_while (seq : Token.t Seq.t) =
  let+ rest = expect_left_paren seq "'while'" in
  let+ expr, rest = Expr.parse rest in
  let+ rest = expect_right_paren rest "condition" in
  let+ body, rest = parse_single rest in
  Ok (STWhile (expr, body), rest)

and parse_for (seq : Token.t Seq.t) =
  let+ rest = expect_left_paren seq "'for'" in
  let+ init, rest =
    match rest () with
    | Seq.Cons ({ tt = Token.Semicolon; _ }, tl) -> Ok (None, tl)
    | Seq.Cons ({ tt = Token.Reserved Token.VarKeyword; _ }, tl) ->
        let+ init, rest = parse_var_decl tl in
        Ok (Some init, rest)
    | _ ->
        let+ init, rest = parse_expr rest in
        Ok (Some init, rest)
  in
  let+ cond, rest =
    match rest () with
    | Seq.Cons (({ tt = Token.Semicolon; _ } as hd), tl) ->
        Ok (Expr.Literal (Expr.ExpToken.exp_token hd (Expr.LBool true)), tl)
    | _ ->
        let+ cond, rest = Expr.parse rest in
        Ok (cond, rest)
  in
  let+ rest = expect_semicolon rest "loop condition" in
  let+ step, rest =
    match rest () with
    | Seq.Cons ({ tt = Token.RightParen; _ }, _) ->
        Ok
          ( None,
            rest
            (* the right parenthesis
               shouldn't be skipped *) )
    | _ ->
        let+ step, rest = Expr.parse rest in
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

and parse_fun (seq : Token.t Seq.t) kind =
  let rec parse_params seq params =
    match Parsing.expect_right_paren_opt seq with
    | Some _ -> Ok (List.[], seq) (* we shouldn't skip the right paren *)
    | None -> (
        let+ param, rest = Parsing.expect_ident seq "Expect parameter name" in
        let params = List.(params @ [ param ]) in
        match Parsing.expect_comma_opt rest with
        | Some rest -> parse_params rest params
        | _ -> Ok (params, rest))
  in
  let+ name, rest =
    Parsing.expect_ident seq (Printf.sprintf "Expect %s name" kind)
  in
  let+ rest = Parsing.expect_left_paren rest (Printf.sprintf "%s name" kind) in
  let+ params, rest = parse_params rest List.[] in
  let+ rest = Parsing.expect_right_paren rest "parameters" in
  let+ rest =
    Parsing.expect_left_brace rest (Printf.sprintf "before %s body" kind)
  in
  let+ body, rest = parse_block rest in
  Ok (STFn (name, params, body), rest)

and parse_return (seq : Token.t Seq.t) =
  let+ expr, rest = Expr.parse seq in
  let+ rest = Parsing.expect_semicolon rest "return value" in
  Ok (STRet expr, rest)
