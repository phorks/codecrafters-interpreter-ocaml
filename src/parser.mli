type unop = NegUnop | NotUnop

type binop =
  | PlusBinop
  | MinusBinop
  | StarBinop
  | SlashBinop
  | EqBinop
  | NeqBinop
  | LtBinop
  | LeqBinop
  | GtBinop
  | GeqBinop
  | LogOrBinop
  | LogAndBinop

type literal =
  | LNil
  | LBool of bool
  | LNum of float
  | LStr of string
  | LVar of string

module ExpToken : sig
  type 'a t = { token : Scanner.token; kind : 'a }

  val exp_token : Scanner.token -> 'a -> 'a t
  val pretty_print : 'a t -> string
end

type exp =
  | Literal of literal ExpToken.t
  | Unary of unop ExpToken.t * exp
  | Binary of binop ExpToken.t * exp * exp
  | Grouping of exp
  | Assignment of string * exp

val pretty_print : exp -> string

type syntax_error = SyntaxError of (Scanner.token option * string)

val syntax_error_to_string : syntax_error -> string

val parse_expr :
  Scanner.token Seq.t -> (exp * Scanner.token Seq.t, syntax_error) result
