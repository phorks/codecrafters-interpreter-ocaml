type native_fn = NFClock

module Env = Value.Env

let native_fns = List.[ NFClock ]
let nf_arity = function NFClock -> 0
let nf_name = function NFClock -> "clock"

let nf_call = function
  | NFClock -> fun (_, env) -> Ok (Value.VNum (Unix.time ()), env)

let global_env =
  let rec aux env fns =
    match fns with
    | List.[] -> env
    | List.(hd :: tl) ->
        let env =
          Env.define (nf_name hd)
            (Value.VCallable (nf_arity hd, nf_call hd))
            env
        in
        aux env tl
  in
  let globals = aux Env.empty native_fns in
  Env.empty_with_parent globals

let extract_globals env = Value.Env.root env

let replace_globals target source =
  let root = extract_globals source in
  Value.Env.replace_root target root
