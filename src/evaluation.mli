type runtime_error = RuntimeError of string

val runtime_error_to_string : runtime_error -> string

type value = VNil | VBool of bool | VNum of float | VStr of string

module Environment : sig
  type t

  val empty : t
  val get : string -> t -> (value, runtime_error) result
  val define : string -> value -> t -> t
end

val pretty_print : value -> string
val eval : Parser.exp -> Environment.t -> (value, runtime_error) result
