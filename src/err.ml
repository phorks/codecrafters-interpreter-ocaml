type syntax_error = SyntaxError of (Token.t option * string)

let syntax_error_to_string err =
  match err with
  | SyntaxError (token, msg) ->
      let line, at_msg =
        match token with
        | Some token -> (token.line, Token.token_error_at_msg token)
        | None -> (-1, "end")
      in
      Printf.sprintf "[line %d] Error at %s: %s.\n" line at_msg msg

type runtime_error = RuntimeError of string

let runtime_error_to_string e = match e with RuntimeError s -> s
