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
  | LogOrBinop
  | LogAndBinop

type literal =
  | LNil
  | LBool of bool
  | LNum of float
  | LStr of string
  | LVar of string

module ExpToken = struct
  type 'a t = { token : Token.t; kind : 'a }

  let exp_token token kind = { token; kind }
  let pretty_print exp_token = Token.pretty_print_tt exp_token.token.tt
end

type t =
  | Literal of literal ExpToken.t
  | Unary of unop ExpToken.t * t
  | Binary of binop ExpToken.t * t * t
  | Grouping of t
  | Assignment of string * t
  | Call of t * t list

let rec pretty_print exp =
  match exp with
  | Literal l -> ExpToken.pretty_print l
  | Unary (op, x) ->
      Printf.sprintf "(%s %s)" (ExpToken.pretty_print op) (pretty_print x)
  | Binary (op, x, y) ->
      Printf.sprintf "(%s %s %s)" (ExpToken.pretty_print op) (pretty_print x)
        (pretty_print y)
  | Grouping e -> Printf.sprintf "(group %s)" (pretty_print e)
  | Assignment (name, expr) ->
      Printf.sprintf "(= %s %s)" name (pretty_print expr)
  | Call (expr, args) ->
      let rec aux args =
        match args with
        | List.[] -> ""
        | List.(hd :: tl) -> Printf.sprintf "%s; %s" (pretty_print hd) (aux tl)
      in
      Printf.sprintf "(call %s [%s])" (pretty_print expr) (aux args)

let seq_hd_opt seq =
  match seq () with Seq.Nil -> None | Seq.Cons (hd, _) -> Some hd

let seq_tl seq = match seq () with Seq.Nil -> seq | Seq.Cons (_, tl) -> tl
let ( let* ) = Option.bind
let ( let+ ) = Result.bind

let match_assignment_op (seq : Token.t Seq.t) =
  let* hd = seq_hd_opt seq in
  match hd.tt with Token.Equal -> Some hd | _ -> None

let match_equality_op (seq : Token.t Seq.t) =
  let* hd = seq_hd_opt seq in
  match hd.tt with
  | EqualEqual -> Some (ExpToken.exp_token hd EqBinop)
  | BangEqual -> Some (ExpToken.exp_token hd NeqBinop)
  | _ -> None

let match_comparison_op (seq : Token.t Seq.t) =
  let* hd = seq_hd_opt seq in
  match hd.tt with
  | Less -> Some (ExpToken.exp_token hd LtBinop)
  | LessEqual -> Some (ExpToken.exp_token hd LeqBinop)
  | Greater -> Some (ExpToken.exp_token hd GtBinop)
  | GreaterEqual -> Some (ExpToken.exp_token hd GeqBinop)
  | _ -> None

let match_term_op (seq : Token.t Seq.t) =
  let* hd = seq_hd_opt seq in
  match hd.tt with
  | Minus -> Some (ExpToken.exp_token hd MinusBinop)
  | Plus -> Some (ExpToken.exp_token hd PlusBinop)
  | _ -> None

let match_factor_op (seq : Token.t Seq.t) =
  let* hd = seq_hd_opt seq in
  match hd.tt with
  | Star -> Some (ExpToken.exp_token hd StarBinop)
  | Slash -> Some (ExpToken.exp_token hd SlashBinop)
  | _ -> None

let match_unary_op (seq : Token.t Seq.t) =
  let* hd = seq_hd_opt seq in
  match hd.tt with
  | Bang -> Some (ExpToken.exp_token hd NotUnop)
  | Minus -> Some (ExpToken.exp_token hd NegUnop)
  | _ -> None

let match_logical_or_op (seq : Token.t Seq.t) =
  let* hd = seq_hd_opt seq in
  match hd.tt with
  | Reserved OrKeyword -> Some (ExpToken.exp_token hd LogOrBinop)
  | _ -> None

let match_logical_and_op (seq : Token.t Seq.t) =
  let* hd = seq_hd_opt seq in
  match hd.tt with
  | Reserved AndKeyword -> Some (ExpToken.exp_token hd LogAndBinop)
  | _ -> None

let rec parse seq = parse_assignment seq

and parse_assignment (seq : Token.t Seq.t) =
  let+ left, rest = parse_or seq in
  match match_assignment_op rest with
  | Some op -> (
      let+ right, rest = parse_assignment (seq_tl rest) in
      match left with
      | Literal l -> (
          match l.kind with
          | LVar name -> Ok (Assignment (name, right), rest)
          | _ -> Error (Err.SyntaxError (Some op, "Invalid assignment target."))
          )
      | _ -> Error (Err.SyntaxError (Some op, "Invalid assignment target.")))
  | None -> Ok (left, rest)

and parse_or seq =
  let+ expr, rest = parse_and seq in

  let rec aux seq left =
    match match_logical_or_op seq with
    | Some op ->
        let+ right, rest = parse_and (seq_tl seq) in
        let expr = Binary (op, left, right) in
        aux rest expr
    | None -> Ok (left, seq)
  in
  aux rest expr

and parse_and seq =
  let+ expr, rest = parse_equality seq in

  let rec aux seq left =
    match match_logical_and_op seq with
    | Some op ->
        let+ right, rest = parse_equality (seq_tl seq) in
        let expr = Binary (op, left, right) in
        aux rest expr
    | None -> Ok (left, seq)
  in
  aux rest expr

and parse_equality seq =
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

and parse_unary (seq : Token.t Seq.t) =
  match match_unary_op seq with
  | Some op ->
      let+ right, rest = parse_unary (seq_tl seq) in
      Ok (Unary (op, right), rest)
  | None -> parse_call seq

and parse_call (seq : Token.t Seq.t) =
  let+ expr, rest = parse_primary seq in
  let rec parse_args seq args =
    match Parsing.expect_right_paren_opt seq with
    | Some _ -> Ok (List.[], seq) (* we shouldn't skip the right paren *)
    | None -> (
        let+ arg, rest = parse seq in
        match Parsing.expect_comma_opt rest with
        | Some rest -> parse_args rest List.(args @ [ arg ])
        | _ -> Ok (args, rest))
  in
  let rec aux seq left =
    match Parsing.expect_left_paren_opt seq with
    | Some rest ->
        let+ args, rest = parse_args rest List.[] in
        let+ rest = Parsing.expect_right_paren rest "arguments" in
        aux rest (Call (left, args))
    | _ -> Ok (left, seq)
  in
  aux rest expr

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
      | Ident s -> Ok (Literal (ExpToken.exp_token hd (LVar s)), seq_tl seq)
      | LeftParen -> (
          let+ inner, rest = parse_assignment (seq_tl seq) in
          let+ hd =
            Option.to_result (seq_hd_opt rest)
              ~none:(Err.SyntaxError (None, "Expect expression"))
          in
          match hd.tt with
          | RightParen -> Ok (Grouping inner, seq_tl rest)
          | _ -> Error (Err.SyntaxError (Some hd, "Expect expression")))
      | _ -> Error (SyntaxError (Some hd, "Expect expression")))
  | None -> Error (SyntaxError (None, "Expect expression"))
