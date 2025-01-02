type token_type =
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Star
  | BangEqual
  | Bang
  | Equal
  | EqualEqual
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | Slash
  | Str of string
  | Eof

let tt_string (tt : token_type) : string =
  match tt with
  | LeftParen -> "LEFT_PAREN"
  | RightParen -> "RIGHT_PAREN"
  | LeftBrace -> "LEFT_BRACE"
  | RightBrace -> "RIGHT_BRACE"
  | Comma -> "COMMA"
  | Dot -> "DOT"
  | Minus -> "MINUS"
  | Plus -> "PLUS"
  | Semicolon -> "SEMICOLON"
  | Star -> "STAR"
  | BangEqual -> "BANG_EQUAL"
  | Bang -> "BANG"
  | Equal -> "EQUAL"
  | EqualEqual -> "EQUAL_EQUAL"
  | Less -> "LESS"
  | LessEqual -> "LESS_EQUAL"
  | Greater -> "GREATER"
  | GreaterEqual -> "GREATER_EQUAL"
  | Slash -> "Slash"
  | Str _ -> "STRING"
  | Eof -> "EOF"

let tt_literal (tt : token_type) : string =
  match tt with Str str -> str | _ -> "null"

type token = { tt : token_type; line : int; lexeme : string }
type lexical_error = { msg : string; line : int }
type scan_result = HadError | Successful
type token_result = (token, lexical_error) result

let token_result_line (res : token_result) =
  match res with Error err -> err.line | Ok token -> token.line

module type SCANNER = sig
  type input

  val scanner : string -> input
  val scan : input -> token_result List.t * scan_result
end

module Scanner : SCANNER = struct
  type input = char Seq.t
  type advance_result = { token : token_result; rest : char Seq.t option }

  let scanner str = String.to_seq str

  let match_double rest next =
    match rest () with
    | Seq.Nil -> None
    | Seq.Cons (hd, tl) -> if hd == next then Some tl else None

  let report_ok tt line lexeme rest =
    { token = Ok { tt; lexeme; line }; rest = Some rest }

  let report_double rest hd next tt_single tt_double line =
    match match_double rest next with
    | None ->
        {
          token = Ok { tt = tt_single; lexeme = Char.escaped hd; line };
          rest = Some rest;
        }
    | Some rest' ->
        {
          token =
            Ok
              {
                tt = tt_double;
                lexeme = Char.escaped hd ^ Char.escaped next;
                line;
              };
          rest = Some rest';
        }

  let report_single tt line ch rest =
    { token = Ok { tt; lexeme = Char.escaped ch; line }; rest = Some rest }

  let report_error msg line rest =
    { token = Error { msg; line }; rest = Some rest }

  let match_string_literal seq =
    let rec aux seq literal =
      match seq () with
      | Seq.Nil -> (None, seq)
      | Seq.Cons (hd, tl) -> (
          if hd == '"' then (Some literal, tl)
          else
            match aux tl (literal ^ Char.escaped hd) with
            | None, tl' -> (None, tl')
            | Some s, tl' -> (Some s, tl'))
    in
    aux seq String.empty

  let rec advance_aux seq line : advance_result =
    match seq () with
    | Seq.Nil -> { token = Ok { tt = Eof; lexeme = ""; line }; rest = None }
    | Seq.Cons (hd, tl) -> (
        match hd with
        | '(' -> report_single LeftParen line hd tl
        | ')' -> report_single RightParen line hd tl
        | '{' -> report_single LeftBrace line hd tl
        | '}' -> report_single RightBrace line hd tl
        | ',' -> report_single Comma line hd tl
        | '.' -> report_single Dot line hd tl
        | '-' -> report_single Minus line hd tl
        | '+' -> report_single Plus line hd tl
        | ';' -> report_single Comma line hd tl
        | '*' -> report_single Star line hd tl
        | '!' -> report_double tl hd '=' Bang BangEqual line
        | '=' -> report_double tl hd '=' Equal EqualEqual line
        | '<' -> report_double tl hd '=' Less LessEqual line
        | '>' -> report_double tl hd '=' Greater GreaterEqual line
        | '/' -> (
            match match_double tl '/' with
            | None -> report_single Slash line hd tl
            | Some tl' ->
                advance_aux
                  (Seq.drop 1 (Seq.drop_while (fun ch -> ch <> '\n') tl'))
                  (line + 1))
        | '"' -> (
            let literal, tl' = match_string_literal tl in
            match literal with
            | None -> report_error "Unterminated string." line tl'
            | Some literal ->
                report_ok (Str literal) line
                  (Printf.sprintf "\"%s\"" literal)
                  tl')
        | ' ' | '\r' | '\t' -> advance_aux tl line
        | '\n' -> advance_aux tl (line + 1)
        | _ ->
            report_error (Printf.sprintf "Unexpected character: %c" hd) line tl)

  let scan seq =
    let rec scan_aux seq line =
      let res = advance_aux seq line in
      match res.rest with
      | None -> (List.[ res.token ], Result.is_ok res.token)
      | Some rest ->
          let rest', is_rest_ok = scan_aux rest (token_result_line res.token) in
          (List.(res.token :: rest'), Result.is_ok res.token && is_rest_ok)
    in
    let l, is_ok = scan_aux seq 1 in
    (l, if is_ok then Successful else HadError)
end
