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
  | Number of float
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
  | Number _ -> "NUMBER"
  | Eof -> "EOF"

let tt_literal (tt : token_type) : string =
  match tt with
  | Str str -> str
  | Number num ->
      if Float.is_integer num then Printf.sprintf "%.1f" num
      else Printf.sprintf "%.15g" num
  | _ -> "null"

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
  let is_digit = function '0' .. '9' -> true | _ -> false

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

  let match_string_literal seq line =
    let rec aux seq literal line =
      match seq () with
      | Seq.Nil -> (None, seq, line)
      | Seq.Cons (hd, tl) -> (
          if hd == '"' then (Some literal, tl, line)
          else
            let rest, line =
              if hd == '\n' then (literal, line + 1)
              else (literal ^ Char.escaped hd, line)
            in
            match aux tl rest line with
            | None, tl', line' -> (None, tl', line')
            | Some s, tl', line' -> (Some s, tl', line'))
    in
    aux seq String.empty line

  let report_number_literal seq first_digit line =
    let rec aux seq literal saw_point =
      match seq () with
      | Seq.Nil -> (literal, seq)
      | Seq.Cons (hd, tl) ->
          if is_digit hd then aux tl (literal ^ Char.escaped hd) saw_point
          else if hd == '.' && not saw_point then
            aux tl (literal ^ Char.escaped hd) true
          else (literal, tl)
    in
    let literal, seq' = aux seq (Char.escaped first_digit) false in
    {
      token =
        Ok { tt = Number (Float.of_string literal); lexeme = literal; line };
      rest = Some seq';
    }

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
            let literal, tl', line' = match_string_literal tl line in
            match literal with
            | None -> report_error "Unterminated string." line tl'
            | Some literal ->
                report_ok (Str literal) line'
                  (Printf.sprintf "\"%s\"" literal)
                  tl')
        | ' ' | '\r' | '\t' -> advance_aux tl line
        | '\n' -> advance_aux tl (line + 1)
        | ch ->
            if is_digit ch then report_number_literal tl ch line
            else
              report_error
                (Printf.sprintf "Unexpected character: %c" hd)
                line tl)

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
