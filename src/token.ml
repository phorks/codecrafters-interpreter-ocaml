type reserved =
  | AndKeyword
  | ClassKeyword
  | ElseKeyword
  | FalseKeyword
  | ForKeyword
  | FunKeyword
  | IfKeyword
  | NilKeyword
  | OrKeyword
  | PrintKeyword
  | ReturnKeyword
  | SuperKeyword
  | ThisKeyword
  | TrueKeyword
  | VarKeyword
  | WhileKeyword

let reserved_to_string r =
  match r with
  | AndKeyword -> "AND"
  | ClassKeyword -> "CLASS"
  | ElseKeyword -> "ELSE"
  | FalseKeyword -> "FALSE"
  | ForKeyword -> "FOR"
  | FunKeyword -> "FUN"
  | IfKeyword -> "IF"
  | NilKeyword -> "NIL"
  | OrKeyword -> "OR"
  | PrintKeyword -> "PRINT"
  | ReturnKeyword -> "RETURN"
  | SuperKeyword -> "SUPER"
  | ThisKeyword -> "THIS"
  | TrueKeyword -> "TRUE"
  | VarKeyword -> "VAR"
  | WhileKeyword -> "WHILE"

let reserved_of_string_opt s =
  match s with
  | "and" -> Some AndKeyword
  | "class" -> Some ClassKeyword
  | "else" -> Some ElseKeyword
  | "false" -> Some FalseKeyword
  | "for" -> Some ForKeyword
  | "fun" -> Some FunKeyword
  | "if" -> Some IfKeyword
  | "nil" -> Some NilKeyword
  | "or" -> Some OrKeyword
  | "print" -> Some PrintKeyword
  | "return" -> Some ReturnKeyword
  | "super" -> Some SuperKeyword
  | "this" -> Some ThisKeyword
  | "true" -> Some TrueKeyword
  | "var" -> Some VarKeyword
  | "while" -> Some WhileKeyword
  | _ -> None

let pretty_print_reserved r =
  match r with
  | AndKeyword -> "and"
  | ClassKeyword -> "class"
  | ElseKeyword -> "else"
  | FalseKeyword -> "false"
  | ForKeyword -> "for"
  | FunKeyword -> "fun"
  | IfKeyword -> "if"
  | NilKeyword -> "nil"
  | OrKeyword -> "or"
  | PrintKeyword -> "print"
  | ReturnKeyword -> "return"
  | SuperKeyword -> "super"
  | ThisKeyword -> "this"
  | TrueKeyword -> "true"
  | VarKeyword -> "var"
  | WhileKeyword -> "while"

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
  | Num of float
  | Ident of string
  | Reserved of reserved
  | Eof

let pretty_print_tt = function
  | LeftParen -> "("
  | RightParen -> ")"
  | LeftBrace -> "{"
  | RightBrace -> "}"
  | Comma -> ","
  | Dot -> "."
  | Minus -> "-"
  | Plus -> "+"
  | Semicolon -> ";"
  | Star -> "*"
  | BangEqual -> "!="
  | Bang -> "!"
  | Equal -> "="
  | EqualEqual -> "=="
  | Less -> "<"
  | LessEqual -> "<="
  | Greater -> ">"
  | GreaterEqual -> ">="
  | Slash -> "/"
  | Str str -> str
  | Num num -> Common.float_to_string num
  | Ident name -> name
  | Reserved r -> pretty_print_reserved r
  | Eof -> ""

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
  | Slash -> "SLASH"
  | Str _ -> "STRING"
  | Num _ -> "NUMBER"
  | Ident _ -> "IDENTIFIER"
  | Reserved r -> reserved_to_string r
  | Eof -> "EOF"

let tt_literal (tt : token_type) : string =
  match tt with
  | Str str -> str
  | Num num -> Common.float_to_string num
  | _ -> "null"

type t = { tt : token_type; line : int; lexeme : string }
type lexical_error = { msg : string; line : int }
type scan_result = HadError | Successful
type token_result = (t, lexical_error) result

let lexical_error_to_string err =
  Printf.sprintf "[line %d] Error: %s\n" err.line err.msg

let token_result_line (res : token_result) =
  match res with Error err -> err.line | Ok token -> token.line

let token_error_at_msg token : string =
  match token.tt with Eof -> "end" | _ -> Printf.sprintf "'%s'" token.lexeme

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
  let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false
  let is_alphanumeric ch = is_alpha ch || is_digit ch
  let concat str ch = str ^ String.make 1 ch

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
            let rest = literal ^ String.make 1 hd in
            let line = if hd == '\n' then line + 1 else line in
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
          else (literal, seq)
    in
    let literal, seq' = aux seq (Char.escaped first_digit) false in
    {
      token = Ok { tt = Num (Float.of_string literal); lexeme = literal; line };
      rest = Some seq';
    }

  let report_identifier_or_reserved seq first_letter line =
    let rec aux seq name =
      match seq () with
      | Seq.Nil -> (name, seq)
      | Seq.Cons (hd, tl) ->
          if is_alphanumeric hd then aux tl (concat name hd) else (name, seq)
    in
    let name, seq' = aux seq (Char.escaped first_letter) in
    let tt =
      match reserved_of_string_opt name with
      | None -> Ident name
      | Some r -> Reserved r
    in
    { token = Ok { tt; lexeme = name; line }; rest = Some seq' }

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
        | ';' -> report_single Semicolon line hd tl
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
            else if is_alpha ch then report_identifier_or_reserved tl ch line
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

let print_hd (seq : t Seq.t) =
  let msg =
    match seq () with
    | Seq.Nil -> "Empty"
    | Seq.Cons (hd, _) -> pretty_print_tt hd.tt
  in
  Printf.printf "%s -> " msg

let print_seq (seq : t Seq.t) =
  let rec aux (seq : t Seq.t) =
    match seq () with
    | Seq.Nil -> ""
    | Seq.Cons (hd, tl) ->
        Printf.sprintf "%s %s" (pretty_print_tt hd.tt) (aux tl)
  in
  Printf.printf "%s\n" (aux seq)
