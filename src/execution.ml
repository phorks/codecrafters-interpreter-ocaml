open Statements

let exec_stmt stmt =
  match stmt with
  | STExpression _ -> Ok ()
  | STPrint expr ->
      let+ v = Evaluation.eval expr in
      Printf.printf "%s" (Evaluation.pretty_print v);
      Ok ()

let rec exec stmts =
  match stmts with
  | List.[] -> Ok ()
  | List.(hd :: tl) ->
      let+ _ = exec_stmt hd in
      exec tl
