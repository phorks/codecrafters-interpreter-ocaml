type literal =
  | NumberLiteral of float
  | StringLiteral of string
  | TrueLiteral
  | FalseLiteral
  | NilLiteral

let pretty_print_literal = function
  | NumberLiteral num -> Common.float_to_string num
  | StringLiteral s -> s
  | TrueLiteral -> "true"
  | FalseLiteral -> "false"
  | NilLiteral -> "nil"

type binary_operator =
  | EqBinop
  | NeqBinop
  | LtBinop
  | LeqBinop
  | GtBinop
  | GeqBinop
  | PlusBinop
  | MinusBinop
  | MultiplyBinop
  | DivideBinop

let pretty_print_binop = function
  | EqBinop -> "=="
  | NeqBinop -> "!="
  | LtBinop -> "<"
  | LeqBinop -> "<="
  | GtBinop -> ">"
  | GeqBinop -> ">="
  | PlusBinop -> "+"
  | MinusBinop -> "-"
  | MultiplyBinop -> "*"
  | DivideBinop -> "/"

type unary_operator = NegativeUnop | NotUnop

let pretty_print_unop = function NegativeUnop -> "-" | NotUnop -> "!"

type exp =
  | Literal of literal
  | Unary of unary_operator * exp
  | Binary of binary_operator * exp * exp
  | Grouping of exp

let rec pretty_print exp =
  match exp with
  | Literal l -> pretty_print_literal l
  | Unary (op, x) ->
      Printf.sprintf "(%s %s)" (pretty_print_unop op) (pretty_print x)
  | Binary (op, x, y) ->
      Printf.sprintf "(%s %s %s)" (pretty_print_binop op) (pretty_print x)
        (pretty_print y)
  | Grouping e -> Printf.sprintf "(group %s)" (pretty_print e)

type stream = Scanner.token Seq.t

let rec match_token stream types =
  match stream with
  | Seq.Nil -> None
  | Seq.Cons (hd, tl) ->
      if List.fold_left ( || ) false (List.map (fun tt -> hd = tt) types) then
        Some tl
      else None
      
let seq_hd_opt seq =
  match seq with
  | Seq.Nil -> None
  | Seq.Cons(hd, _) -> Some hd
  
let (let*) = Option.bind
      
let match_equality_op stream =
  let* hd = seq_hd_opt stream in
  match hd with
  | Scanner.EqualEqual -> Some EqBinop
  | Scanner.BangEqual -> Some NeqBinop
  | _ -> None
  
let match_comparison_op stream =
  let* hd = seq_hd_opt stream in
  match hd with
  | Scanner.Less -> Some LtBinop
  | Scanner.LessEqual -> Some LeqBinop
  | Scanner.Greater -> Some GtBinop
  | Scanner.GreaterEqual -> Some GeqBinop
  | _ -> None
  
let match_term_op stream = 
  let* hd = seq_hd_opt stream in
  match hd with
  | Scanner.Minus -> MinusBinop
  | Scanner.Plus -> PlusBinop
  
      
let rec parse_equality stream =
  let expr = parse_comparison () in 
  
