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

let rec parse_equality stream =
  let* expr = parse_comparison stream in
  let rec aux seq left =
    match match_equality_op seq with
    | Some op ->
        let* right = parse_comparison (seq_tl stream) in
        let expr = Binary (op, left, right) in
        aux (seq_tl seq) expr
    | None -> Some left
  in
  aux stream expr

and parse_comparison stream =
  let* expr = parse_term stream in
  let rec aux seq left =
    match match_comparison_op seq with
    | Some op ->
        let* right = parse_term (seq_tl stream) in
        let expr = Binary (op, left, right) in
        aux (seq_tl seq) expr
    | None -> Some left
  in
  aux stream expr

and parse_term stream =
  let* expr = parse_factor stream in
  let rec aux seq left =
    match match_factor_op seq with
    | Some op ->
        let* right = parse_factor (seq_tl stream) in
        let expr = Binary (op, left, right) in
        aux (seq_tl seq) expr
    | None -> Some left
  in
  aux stream expr

and parse_factor stream =
  let* expr = parse_unary stream in
  let rec aux seq left =
    match match_factor_op seq with
    | Some op ->
        let* right = parse_unary (seq_tl stream) in
        let expr = Binary (op, left, right) in
        aux (seq_tl seq) expr
    | None -> Some left
  in
  aux stream expr

and parse_unary stream =
  match match_unary_op stream with
  | Some op ->
      let* right = parse_unary (seq_tl stream) in
      Some (Unary (op, right))
  | None -> parse_primary stream

and parse_primary stream =
  match seq_hd_opt stream with
  | Some hd -> (
      match hd.tt with
      | Reserved FalseKeyword
      | Reserved TrueKeyword
      | Reserved NilKeyword
      | Number _ | Str _ ->
          Some (Literal hd)
      | _ -> None)
  | None -> None

let parse_expr stream = parse_equality stream
