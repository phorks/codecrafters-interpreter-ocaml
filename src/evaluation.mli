type value = VNil | VBool of bool | VNum of float | VStr of string

val pretty_print : value -> string

type runtime_error = REOperandType of string

val runtime_error_to_string : runtime_error -> string
val eval : Parser.exp -> (value, runtime_error) result
