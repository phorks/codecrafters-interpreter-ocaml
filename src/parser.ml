open Scanner

type exp =
  | Literal of token
  | Unary of token * exp
  | Binary of token * exp * exp
  | Grouping of exp

let rec pretty_print exp =
  match exp with
  | Literal l -> pretty_print_tt l.tt
  | Unary (op, x) ->
      Printf.sprintf "(%s %s)" (pretty_print_tt op.tt) (pretty_print x)
  | Binary (op, x, y) ->
      Printf.sprintf "(%s %s %s)" (pretty_print_tt op.tt) (pretty_print x)
        (pretty_print y)
  | Grouping e -> Printf.sprintf "(group %s)" (pretty_print e)

type stream = token Seq.t

let seq_hd_opt seq =
  match seq with Seq.Nil -> None | Seq.Cons (hd, _) -> Some hd

let seq_tl seq = match seq with Seq.Nil -> Seq.Nil | Seq.Cons (_, tl) -> tl ()
let ( let* ) = Option.bind

let match_equality_op stream =
  let* hd = seq_hd_opt stream in
  match hd.tt with EqualEqual | BangEqual -> Some hd | _ -> None

let match_comparison_op stream =
  let* hd = seq_hd_opt stream in
  match hd.tt with
  | Less | LessEqual | Greater | GreaterEqual -> Some hd
  | _ -> None

let match_term_op stream =
  let* hd = seq_hd_opt stream in
  match hd.tt with Minus | Plus -> Some hd | _ -> None

let match_factor_op stream =
  let* hd = seq_hd_opt stream in
  match hd.tt with Star | Slash -> Some hd | _ -> None

let match_unary_op stream =
  let* hd = seq_hd_opt stream in
  match hd.tt with Bang | Minus -> Some hd | _ -> None

let print_hd stream =
  match seq_hd_opt stream with
  | Some hd ->
      let _ = Printf.printf "%s" (pretty_print_tt hd.tt) in
      ()
  | None -> Printf.printf "None"

let rec parse_equality seq =
  let* expr, rest = parse_comparison seq in

  let rec aux seq left =
    match match_equality_op seq with
    | Some op ->
        let* right, rest = parse_comparison (seq_tl seq) in
        let expr = Binary (op, left, right) in
        aux rest expr
    | None -> Some (left, seq)
  in
  aux rest expr

and parse_comparison seq =
  let* expr, rest = parse_term seq in
  let rec aux seq left =
    match match_comparison_op seq with
    | Some op ->
        let* right, rest = parse_term (seq_tl seq) in
        let expr = Binary (op, left, right) in
        aux rest expr
    | None -> Some (left, seq)
  in
  aux rest expr

and parse_term seq =
  let* expr, rest = parse_factor seq in
  let rec aux seq left =
    match match_term_op seq with
    | Some op ->
        let* right, rest = parse_factor (seq_tl seq) in
        let expr = Binary (op, left, right) in
        aux rest expr
    | None -> Some (left, seq)
  in
  aux rest expr

and parse_factor seq =
  let* expr, rest = parse_unary seq in
  let rec aux seq left =
    match match_factor_op seq with
    | Some op ->
        let* right, rest = parse_unary (seq_tl seq) in
        let expr = Binary (op, left, right) in
        aux rest expr
    | None -> Some (left, seq)
  in
  aux rest expr

and parse_unary seq =
  match match_unary_op seq with
  | Some op ->
      let* right, rest = parse_unary (seq_tl seq) in
      Some (Unary (op, right), rest)
  | None -> parse_primary seq

and parse_primary seq =
  match seq_hd_opt seq with
  | Some hd -> (
      match hd.tt with
      | Reserved FalseKeyword
      | Reserved TrueKeyword
      | Reserved NilKeyword
      | Number _ | Str _ ->
          Some (Literal hd, seq_tl seq)
      | LeftParen -> (
          let* inner, rest = parse_equality (seq_tl seq) in
          let* hd = seq_hd_opt rest in
          match hd.tt with
          | RightParen -> Some (Grouping inner, seq_tl rest)
          | _ -> None)
      | _ -> None)
  | None -> None

let parse_expr stream = parse_equality stream
