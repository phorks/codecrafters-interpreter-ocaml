type t =
  | VNil
  | VBool of bool
  | VNum of float
  | VStr of string
  | VCallable of
      string option
      * int
      * (t list * env -> (t * env, Err.runtime_error) result)

and env

val truth : t -> bool
val pretty_print : t -> string

module Env : sig
  val empty : env
  val empty_with_parent : env -> env
  val get : string -> env -> (t, Err.runtime_error) result
  val define : string -> t -> env -> env
  val assign : string -> t -> env -> (env, Err.runtime_error) result
  val parent : env -> env option
  val root : env -> env
  val replace_root : env -> env -> env
end

val eval : Expr.t -> env -> (t * env, Err.runtime_error) result
