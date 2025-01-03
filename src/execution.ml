open Statements
module Env = Evaluation.Environment

let exec_stmt stmt env =
  match stmt with
  | STExpression expr ->
      let+ _ = Evaluation.eval expr env in
      Ok env
  | STPrint expr ->
      let+ v = Evaluation.eval expr env in
      Printf.printf "%s\n" (Evaluation.pretty_print v);
      Ok env
  | STVarDecl (name, init) ->
      let+ v =
        match init with
        | Some init -> Evaluation.eval init env
        | None -> Ok Evaluation.VNil
      in
      Ok (Env.define name v env)

let rec exec stmts env =
  match stmts with
  | List.[] -> Ok ()
  | List.(hd :: tl) ->
      let+ env = exec_stmt hd env in
      exec tl env
