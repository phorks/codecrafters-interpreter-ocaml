type statement = STExpression | STPrint

let parse_print seq = 
  let expr = Parser.parse_expr seq

let rec parse seq = match seq with
  | Seq.Nil -> List.[]
  | Seq.Cons(hd, tl) -> 
    let stmt, rest = (match hd with
    | Scanner.Reserved Scanner.PrintKeyword -> parse_print tl
    | _ 
    |)