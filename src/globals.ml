type native_fn = NFClock

module Env = Value.Env

let native_fns = List.[ NFClock ]
let nf_arity = function NFClock -> 0
let nf_name = function NFClock -> "clock"
let nf_call = function NFClock -> fun _ -> Value.VNum (Unix.time ())

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
  aux Env.empty native_fns
