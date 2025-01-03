open Scanner

type unop = NegUnop | NotUnop

type binop =
  | PlusBinop
  | MinusBinop
  | StarBinop
  | SlashBinop
  | EqBinop
  | NeqBinop
  | LtBinop
  | LeqBinop
  | GtBinop
  | GeqBinop

type literal =
  | LNil
  | LBool of bool
  | LNum of float
  | LStr of string
  | LIdent of string

module ExpToken = struct
  type 'a t = { token : token; kind : 'a }

  let exp_token token kind = { token; kind }
  let pretty_print exp_token = pretty_print_tt exp_token.token.tt
end

type exp =
  | Literal of literal ExpToken.t
  | Unary of unop ExpToken.t * exp
  | Binary of binop ExpToken.t * exp * exp
  | Grouping of exp

let rec pretty_print exp =
  match exp with
  | Literal l -> ExpToken.pretty_print l
  | Unary (op, x) ->
      Printf.sprintf "(%s %s)" (ExpToken.pretty_print op) (pretty_print x)
  | Binary (op, x, y) ->
      Printf.sprintf "(%s %s %s)" (ExpToken.pretty_print op) (pretty_print x)
        (pretty_print y)
  | Grouping e -> Printf.sprintf "(group %s)" (pretty_print e)

type syntax_error = SyntaxError of (token option * string)

let syntax_error_to_string err =
  match err with
  | SyntaxError (token, msg) ->
      let line, at_msg =
        match token with
        | Some token -> (token.line, token_error_at_msg token)
        | None -> (-1, "end")
      in
      Printf.sprintf "[line %d] Error at %s: %s.\n" line at_msg msg

let seq_hd_opt seq =
  match seq with Seq.Nil -> None | Seq.Cons (hd, _) -> Some hd

let seq_tl seq = match seq with Seq.Nil -> Seq.Nil | Seq.Cons (_, tl) -> tl ()
let ( let* ) = Option.bind
let ( let+ ) = Result.bind

let match_equality_op seq =
  let* hd = seq_hd_opt seq in
  match hd.tt with
  | EqualEqual -> Some (ExpToken.exp_token hd EqBinop)
  | BangEqual -> Some (ExpToken.exp_token hd NeqBinop)
  | _ -> None

let match_comparison_op seq =
  let* hd = seq_hd_opt seq in
  match hd.tt with
  | Less -> Some (ExpToken.exp_token hd LtBinop)
  | LessEqual -> Some (ExpToken.exp_token hd LeqBinop)
  | Greater -> Some (ExpToken.exp_token hd GtBinop)
  | GreaterEqual -> Some (ExpToken.exp_token hd GeqBinop)
  | _ -> None

let match_term_op seq =
  let* hd = seq_hd_opt seq in
  match hd.tt with
  | Minus -> Some (ExpToken.exp_token hd MinusBinop)
  | Plus -> Some (ExpToken.exp_token hd PlusBinop)
  | _ -> None

let match_factor_op seq =
  let* hd = seq_hd_opt seq in
  match hd.tt with
  | Star -> Some (ExpToken.exp_token hd StarBinop)
  | Slash -> Some (ExpToken.exp_token hd SlashBinop)
  | _ -> None

let match_unary_op seq =
  let* hd = seq_hd_opt seq in
  match hd.tt with
  | Bang -> Some (ExpToken.exp_token hd NotUnop)
  | Minus -> Some (ExpToken.exp_token hd NegUnop)
  | _ -> None

let rec parse_equality seq =
  let+ expr, rest = parse_comparison seq in

  let rec aux seq left =
    match match_equality_op seq with
    | Some op ->
        let+ right, rest = parse_comparison (seq_tl seq) in
        let expr = Binary (op, left, right) in
        aux rest expr
    | None -> Ok (left, seq)
  in
  aux rest expr

and parse_comparison seq =
  let+ expr, rest = parse_term seq in
  let rec aux seq left =
    match match_comparison_op seq with
    | Some op ->
        let+ right, rest = parse_term (seq_tl seq) in
        let expr = Binary (op, left, right) in
        aux rest expr
    | None -> Ok (left, seq)
  in
  aux rest expr

and parse_term seq =
  let+ expr, rest = parse_factor seq in
  let rec aux seq left =
    match match_term_op seq with
    | Some op ->
        let+ right, rest = parse_factor (seq_tl seq) in
        let expr = Binary (op, left, right) in
        aux rest expr
    | None -> Ok (left, seq)
  in
  aux rest expr

and parse_factor seq =
  let+ expr, rest = parse_unary seq in
  let rec aux seq left =
    match match_factor_op seq with
    | Some op ->
        let+ right, rest = parse_unary (seq_tl seq) in
        let expr = Binary (op, left, right) in
        aux rest expr
    | None -> Ok (left, seq)
  in
  aux rest expr

and parse_unary seq =
  match match_unary_op seq with
  | Some op ->
      let+ right, rest = parse_unary (seq_tl seq) in
      Ok (Unary (op, right), rest)
  | None -> parse_primary seq

and parse_primary seq =
  match seq_hd_opt seq with
  | Some hd -> (
      match hd.tt with
      | Reserved FalseKeyword ->
          Ok (Literal (ExpToken.exp_token hd (LBool false)), seq_tl seq)
      | Reserved TrueKeyword ->
          Ok (Literal (ExpToken.exp_token hd (LBool true)), seq_tl seq)
      | Reserved NilKeyword ->
          Ok (Literal (ExpToken.exp_token hd LNil), seq_tl seq)
      | Num num -> Ok (Literal (ExpToken.exp_token hd (LNum num)), seq_tl seq)
      | Str s -> Ok (Literal (ExpToken.exp_token hd (LStr s)), seq_tl seq)
      | Ident s -> Ok (Literal (ExpToken.exp_token hd (LIdent s)), seq_tl seq)
      | LeftParen -> (
          let+ inner, rest = parse_equality (seq_tl seq) in
          let+ hd =
            Option.to_result (seq_hd_opt rest)
              ~none:(SyntaxError (None, "Expect expression"))
          in
          match hd.tt with
          | RightParen -> Ok (Grouping inner, seq_tl rest)
          | _ -> Error (SyntaxError (Some hd, "Expect expression")))
      | _ -> Error (SyntaxError (Some hd, "Expect expression")))
  | None -> Error (SyntaxError (None, "Expect expression"))

let parse_expr stream = parse_equality stream
