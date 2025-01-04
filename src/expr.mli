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
  type 'a t = { token : Token.t; kind : 'a }

  val exp_token : Token.t -> 'a -> 'a t
  val pretty_print : 'a t -> string
end

type t =
  | Literal of literal ExpToken.t
  | Unary of unop ExpToken.t * t
  | Binary of binop ExpToken.t * t * t
  | Grouping of t
  | Assignment of string * t
  | Call of t * t list

val pretty_print : t -> string
val parse : Token.t Seq.t -> (t * Token.t Seq.t, Err.syntax_error) result
