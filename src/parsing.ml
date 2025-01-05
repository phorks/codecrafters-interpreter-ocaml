let ( let+ ) = Result.bind

let expect_tt (seq : Token.t Seq.t) err_msg f =
  match seq () with
  | Seq.Nil -> Error (Err.SyntaxError (None, err_msg))
  | Seq.Cons (hd, tl) -> (
      match f hd.tt with
      | Some x -> Ok (x, tl)
      | None -> Error (Err.SyntaxError (Some hd, err_msg)))

let expect_semicolon seq after_what =
  let+ _, rest =
    expect_tt seq (Printf.sprintf "Expect ';' after %s" after_what) (function
      | Token.Semicolon -> Some ()
      | _ -> None)
  in
  Ok rest

let expect_left_paren seq after_what =
  let+ _, rest =
    expect_tt seq (Printf.sprintf "Expect '(' after %s" after_what) (function
      | Token.LeftParen -> Some ()
      | _ -> None)
  in
  Ok rest

let expect_right_paren seq after_what =
  let+ _, rest =
    expect_tt seq (Printf.sprintf "Expect ')' after %s" after_what) (function
      | Token.RightParen -> Some ()
      | _ -> None)
  in
  Ok rest

let expect_left_brace seq where =
  let+ _, rest =
    expect_tt seq (Printf.sprintf "Expect '}' %s" where) (function
      | Token.LeftBrace -> Some ()
      | _ -> None)
  in
  Ok rest

let expect_ident seq err_msg =
  expect_tt seq err_msg (function Token.Ident name -> Some name | _ -> None)

let expect_tt_opt_val (seq : Token.t Seq.t) f =
  match seq () with
  | Seq.Nil -> None
  | Seq.Cons (hd, tl) -> Option.map (fun x -> (x, tl)) (f hd.tt)

let expect_tt_opt (seq : Token.t Seq.t) f =
  match seq () with
  | Seq.Nil -> None
  | Seq.Cons (hd, tl) -> if f hd.tt then Some tl else None

let expect_equal_opt seq =
  expect_tt_opt seq (function Token.Equal -> true | _ -> false)

let expect_left_paren_opt seq =
  expect_tt_opt seq (function Token.LeftParen -> true | _ -> false)

let expect_right_paren_opt seq =
  expect_tt_opt seq (function Token.RightParen -> true | _ -> false)

let expect_comma_opt seq =
  expect_tt_opt seq (function Token.Comma -> true | _ -> false)
