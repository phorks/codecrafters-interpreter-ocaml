let ( let+ ) = Result.bind

module Env = Value.Env

type exec_result = ERReturn of Value.t | ERContinue | ERBreak

let rec exec_stmt stmt env =
  match stmt with
  | Stmt.STExpr expr ->
      let+ _, env = Value.eval expr env in
      Ok (None, env)
  | Stmt.STPrint expr ->
      let+ v, env = Value.eval expr env in
      Printf.printf "%s\n" (Value.pretty_print v);
      Ok (None, env)
  | Stmt.STVarDecl (name, init) ->
      let+ v, env =
        match init with
        | Some init -> Value.eval init env
        | None -> Ok (Value.VNil, env)
      in
      Ok (None, Env.define name v env)
  | Stmt.STBlock stmts ->
      let env = Env.empty_with_parent env in
      let rec aux stmts env =
        match stmts with
        | List.[] -> Ok (None, env)
        | List.(hd :: tl) -> (
            let+ res, env' = exec_stmt hd env in
            match res with Some _ -> Ok (res, env) | _ -> aux tl env')
      in
      let+ res, env' = aux stmts env in
      Ok (res, Option.get (Env.parent env'))
  | Stmt.STIf (expr, body, else_branch) -> (
      let+ cond, env = Value.eval expr env in
      let cond = Value.truth cond in
      if cond then exec_stmt body env
      else
        match else_branch with
        | Some else_branch -> exec_stmt else_branch env
        | None -> Ok (None, env))
  | Stmt.STWhile (expr, body) ->
      let rec aux env =
        let+ cond, env = Value.eval expr env in
        let cond = Value.truth cond in
        if cond then
          let+ res, env = exec_stmt body env in
          match res with
          | None | Some ERContinue -> aux env
          | Some ERBreak -> Ok (None, env)
          | Some (ERReturn _) -> Ok (res, env)
        else Ok (None, env)
      in
      aux env
  | Stmt.STFn (name, params, body) ->
      Ok
        ( None,
          Env.define name
            (Value.VCallable
               ( List.length params,
                 fun (args, env) ->
                   let rec define_args pairs env =
                     match pairs with
                     | List.[] -> env
                     | List.((n, v) :: tl) ->
                         define_args tl (Env.define n v env)
                   in
                   let globals = Globals.extract_globals env in
                   let exec_env = Env.empty_with_parent globals in
                   let exec_env =
                     define_args (List.combine params args) exec_env
                   in
                   let+ res, exec_env = exec_stmt body exec_env in
                   let v =
                     match res with Some (ERReturn v) -> v | _ -> Value.VNil
                   in
                   Ok (v, Globals.replace_globals env exec_env) ))
            env )
  | Stmt.STRet expr ->
      let+ v, rest = Value.eval expr env in
      Ok (Some (ERReturn v), rest)

let rec exec stmts env =
  match stmts with
  | List.[] -> Ok ()
  | List.(hd :: tl) -> (
      let+ res, env = exec_stmt hd env in
      match res with
      | None -> exec tl env
      | _ ->
          Error
            (Err.RuntimeError
               "continue, break, and return should not be used in the main code")
      )
