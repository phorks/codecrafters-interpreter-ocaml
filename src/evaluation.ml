type runtime_error = RuntimeError of string

let runtime_error_to_string e = match e with RuntimeError s -> s

type value = VNil | VBool of bool | VNum of float | VStr of string

module Environment : sig
  type t

  val empty : t
  val get : string -> t -> (value, runtime_error) result
  val define : string -> value -> t -> t
end = struct
  module ValueMap = Map.Make (String)

  type t = value ValueMap.t

  let empty = ValueMap.empty

  let get (name : string) (env : t) =
    match ValueMap.find_opt name env with
    | Some v -> Ok v
    | None ->
        Error (RuntimeError (Printf.sprintf "Undefined variable '%s'." name))

  let define (name : string) (v : value) (env : t) = ValueMap.add name v env
end

let eval_literal l env =
  match l with
  | Parser.LNil -> Ok VNil
  | Parser.LBool b -> Ok (VBool b)
  | Parser.LNum x -> Ok (VNum x)
  | Parser.LStr s -> Ok (VStr s)
  | Parser.LIdent name -> Environment.get name env

let pretty_print v =
  match v with
  | VNil -> "nil"
  | VBool b -> if b then "true" else "false"
  | VNum n -> Common.float_value_to_string n
  | VStr s -> s

let ( let+ ) = Result.bind
let val_truth = function VNil -> false | VBool b -> b | _ -> true
let eval_unary_num v f g = match v with VNum n -> Ok (VNum (f n)) | _ -> g ()
let eval_unary_bool v f = Ok (VBool (f (val_truth v)))

let eval_binary_num v1 v2 f g =
  match (v1, v2) with VNum n1, VNum n2 -> Ok (f n1 n2) | _ -> g ()

(* let eval_binary_bool v1 v2 f = Ok (f (val_truth v1) (val_truth v2)) *)

let eval_binary_str v1 v2 f g =
  match (v1, v2) with VStr s1, VStr s2 -> Ok (f s1 s2) | _ -> g ()

let f_num f n1 n2 = VNum (f n1 n2)

(* let f_bool f b1 b2 = VBool (f b1 b2) *)
let f_str f s1 s2 = VStr (f s1 s2)

let unop_type_err type_str () =
  Error (RuntimeError (Printf.sprintf "Operand must be a %s." type_str))

let binop_type_err type_str () =
  Error (RuntimeError (Printf.sprintf "Operands must be %ss." type_str))

let binop_num_str_err () =
  Error (RuntimeError "Operands must be two numbers or two strings.")

let rec eval expr env =
  match expr with
  | Parser.Literal l -> eval_literal l.kind env
  | Parser.Unary (op, a) -> (
      let+ v = eval a env in
      match op.kind with
      | Parser.NegUnop -> eval_unary_num v Float.neg (unop_type_err "number")
      | Parser.NotUnop -> eval_unary_bool v Bool.not)
  | Binary (op, a, b) -> (
      let+ v1 = eval a env in
      let+ v2 = eval b env in
      match op.kind with
      | PlusBinop ->
          eval_binary_num v1 v2 (f_num Float.add) (fun () ->
              eval_binary_str v1 v2 (f_str String.cat) binop_num_str_err)
      | MinusBinop ->
          eval_binary_num v1 v2 (f_num Float.sub) (binop_type_err "number")
      | StarBinop ->
          eval_binary_num v1 v2 (f_num Float.mul) (binop_type_err "number")
      | SlashBinop ->
          eval_binary_num v1 v2 (f_num Float.div) (binop_type_err "number")
      | EqBinop -> (
          match (v1, v2) with
          | VNil, VNil -> Ok (VBool true)
          | VBool b1, VBool b2 -> Ok (VBool (b1 = b2))
          | VNum n1, VNum n2 -> Ok (VBool (n1 = n2))
          | VStr s1, VStr s2 -> Ok (VBool (s1 = s2))
          | _ -> Ok (VBool false))
      | NeqBinop -> (
          match (v1, v2) with
          | VNil, VNil -> Ok (VBool false)
          | VBool b1, VBool b2 -> Ok (VBool (b1 <> b2))
          | VNum n1, VNum n2 -> Ok (VBool (n1 <> n2))
          | VStr s1, VStr s2 -> Ok (VBool (s1 <> s2))
          | _ -> Ok (VBool true))
      | LtBinop ->
          eval_binary_num v1 v2
            (fun n1 n2 -> VBool (n1 < n2))
            (binop_type_err "number")
      | LeqBinop ->
          eval_binary_num v1 v2
            (fun n1 n2 -> VBool (n1 <= n2))
            (binop_type_err "number")
      | GtBinop ->
          eval_binary_num v1 v2
            (fun n1 n2 -> VBool (n1 > n2))
            (binop_type_err "number")
      | GeqBinop ->
          eval_binary_num v1 v2
            (fun n1 n2 -> VBool (n1 >= n2))
            (binop_type_err "number"))
  | Grouping inner -> eval inner env
