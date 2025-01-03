type statement =
  | STExpression of Parser.exp
  | STPrint of Parser.exp
  | STVarDecl of string * Parser.exp option
  | STBlock of statement list

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

let expect_ident seq err_msg =
  expect_tt seq err_msg (function Scanner.Ident name -> Some name | _ -> None)

let expect_tt_opt (seq : Scanner.token Seq.node) f =
  match seq with
  | Seq.Nil -> None
  | Seq.Cons (hd, tl) -> Option.map (fun x -> (x, tl)) (f hd.tt)

let expect_equal_opt seq =
  expect_tt_opt seq (function Scanner.Equal -> Some () | _ -> None)

let print_hd (seq : Scanner.token Seq.t) =
  let msg =
    match seq () with
    | Seq.Nil -> "Empty"
    | Seq.Cons (hd, _) -> Scanner.pretty_print_tt hd.tt
  in
  Printf.printf "%s -> " msg

let print_seq (seq : Scanner.token Seq.t) =
  let rec aux (seq : Scanner.token Seq.t) =
    match seq () with
    | Seq.Nil -> ""
    | Seq.Cons (hd, tl) ->
        Printf.sprintf "%s %s" (Scanner.pretty_print_tt hd.tt) (aux tl)
  in
  Printf.printf "%s\n" (aux seq)

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
        match Parser.parse_expr tl with
        | Ok (expr, tl) -> Ok (Some expr, tl)
        | Error err -> Error err)
    | None -> Ok (None, rest)
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
          let+ stmt, rest = parse_single hd tl in
          let+ rest = parse rest in
          Ok List.(stmt :: rest))

(* match hd.tt with *)
(* | Scanner.Eof -> Ok List.[] *)
(* | _ -> *)
(*     let+ stmt, rest = *)
(*       match hd.tt with *)
(*       | Scanner.Reserved Scanner.PrintKeyword -> parse_print (tl ()) *)
(*       | Scanner.Reserved Scanner.VarKeyword -> parse_var_decl (tl ()) *)
(*       | Scanner.LeftBrace -> parse_block (tl ()) *)
(*       | _ -> parse_expr (seq ()) *)
(*     in *)
(*     let+ rest = parse rest in *)
(*     Ok List.(stmt :: rest)) *)
and parse_single (token : Scanner.token) rest =
  let+ stmt, rest =
    match token.tt with
    | Scanner.Reserved Scanner.PrintKeyword -> parse_print rest
    | Scanner.Reserved Scanner.VarKeyword -> parse_var_decl rest
    | Scanner.LeftBrace -> parse_block rest
    | _ -> parse_expr (Seq.cons token rest)
  in
  Ok (stmt, rest)

and parse_block (seq : Scanner.token Seq.t) =
  let rec aux (seq : Scanner.token Seq.t) =
    match seq () with
    | Seq.Nil -> Ok (List.[], seq)
    | Seq.Cons (hd, tl) -> (
        match hd.tt with
        | Scanner.Eof ->
            Error (Parser.SyntaxError (Some hd, "Expect '}' after block"))
        | Scanner.RightBrace -> Ok (List.[], tl)
        | _ ->
            let+ stmt, rest = parse_single hd tl in
            let+ stmts, rest = aux rest in
            Ok (List.(stmt :: stmts), rest))
  in
  let+ stmts, rest = aux seq in
  Ok (STBlock stmts, rest)
