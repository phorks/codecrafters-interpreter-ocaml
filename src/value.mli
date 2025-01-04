type t =
  | VNil
  | VBool of bool
  | VNum of float
  | VStr of string
  | VCallable of int * (t list -> t)

val truth : t -> bool
val pretty_print : t -> string

module Env : sig
  type value := t
  type t

  val empty : t
  val empty_with_parent : t -> t
  val get : string -> t -> (value, Err.runtime_error) result
  val define : string -> value -> t -> t
  val assign : string -> value -> t -> (t, Err.runtime_error) result
  val parent : t -> t option
end

val eval : Expr.t -> Env.t -> (t * Env.t, Err.runtime_error) result
