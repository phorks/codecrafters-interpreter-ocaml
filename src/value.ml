let ( let+ ) = Result.bind

exception Unreachable

module ValueMap = Map.Make (String)

type t =
  | VNil
  | VBool of bool
  | VNum of float
  | VStr of string
  | VCallable of int * (t list * env -> (t * env, Err.runtime_error) result)

and env = Node of t ValueMap.t * env option

let truth = function VNil -> false | VBool b -> b | _ -> true

let pretty_print v =
  match v with
  | VNil -> "nil"
  | VBool b -> if b then "true" else "false"
  | VNum n -> Common.float_value_to_string n
  | VStr s -> s
  | VCallable (arity, _) -> Printf.sprintf "callable(arity: %d)" arity

module Env = struct
  let empty = Node (ValueMap.empty, None)
  let empty_with_parent parent : env = Node (ValueMap.empty, Some parent)

  let rec get (name : string) (env : env) =
    match env with
    | Node (map, parent) -> (
        match ValueMap.find_opt name map with
        | Some v -> Ok v
        | None -> (
            match parent with
            | None ->
                Error
                  (Err.RuntimeError
                     (Printf.sprintf "Undefined variable '%s'." name))
            | Some parent -> get name parent))

  let define (name : string) (v : t) (env : env) =
    match env with Node (map, parent) -> Node (ValueMap.add name v map, parent)

  let rec assign (name : string) (v : t) (env : env) =
    match env with
    | Node (map, parent) -> (
        if ValueMap.mem name map then
          Ok (Node (ValueMap.add name v map, parent))
        else
          match parent with
          | None ->
              Error
                (Err.RuntimeError
                   (Printf.sprintf "Undefined variable '%s'." name))
          | Some parent ->
              let+ parent = assign name v parent in
              Ok (Node (map, Some parent)))

  let parent (env : env) = match env with Node (_, parent) -> parent

  let rec root env =
    match env with
    | Node (_, parent) -> (
        match parent with Some parent -> root parent | None -> env)

  let rec replace_root env root =
    match env with
    | Node (map, parent) -> (
        match parent with
        | Some parent -> Node (map, Some (replace_root parent root))
        | _ -> root)
end

let eval_literal l env =
  let+ v =
    match l with
    | Expr.LNil -> Ok VNil
    | Expr.LBool b -> Ok (VBool b)
    | Expr.LNum x -> Ok (VNum x)
    | Expr.LStr s -> Ok (VStr s)
    | Expr.LVar name -> Env.get name env
  in
  Ok (v, env)

let eval_unary_num v env f g =
  match v with VNum n -> Ok (VNum (f n), env) | _ -> g ()

let eval_unary_bool v env f = Ok (VBool (f (truth v)), env)

let eval_binary_num v1 v2 env f g =
  match (v1, v2) with VNum n1, VNum n2 -> Ok (f n1 n2, env) | _ -> g ()

(* let eval_binary_bool v1 v2 f = Ok (f (val_truth v1) (val_truth v2)) *)

let eval_binary_str v1 v2 env f g =
  match (v1, v2) with VStr s1, VStr s2 -> Ok (f s1 s2, env) | _ -> g ()

let f_num f n1 n2 = VNum (f n1 n2)

(* let f_bool f b1 b2 = VBool (f b1 b2) *)
let f_str f s1 s2 = VStr (f s1 s2)

let unop_type_err type_str () =
  Error (Err.RuntimeError (Printf.sprintf "Operand must be a %s." type_str))

let binop_type_err type_str () =
  Error (Err.RuntimeError (Printf.sprintf "Operands must be %ss." type_str))

let binop_num_str_err () =
  Error (Err.RuntimeError "Operands must be two numbers or two strings.")

let rec eval expr env : (t * env, Err.runtime_error) result =
  match expr with
  | Expr.Literal l -> eval_literal l.kind env
  | Expr.Unary (op, a) -> (
      let+ v, env = eval a env in
      match op.kind with
      | Expr.NegUnop -> eval_unary_num v env Float.neg (unop_type_err "number")
      | Expr.NotUnop -> eval_unary_bool v env Bool.not)
  | Binary ({ kind = LogOrBinop; _ }, a, b) ->
      (* and & or need different treatments because of the lazy evaluation *)
      let+ v1, env = eval a env in
      if truth v1 then Ok (v1, env)
      else
        let+ v2, env = eval b env in
        Ok (v2, env)
  | Binary ({ kind = LogAndBinop; _ }, a, b) ->
      let+ v1, env = eval a env in
      if truth v1 then
        let+ v2, env = eval b env in
        Ok (v2, env)
      else Ok (VBool false, env)
  | Binary (op, a, b) -> (
      let+ v1, env = eval a env in
      let+ v2, env = eval b env in
      match op.kind with
      | PlusBinop ->
          eval_binary_num v1 v2 env (f_num Float.add) (fun () ->
              eval_binary_str v1 v2 env (f_str String.cat) binop_num_str_err)
      | MinusBinop ->
          eval_binary_num v1 v2 env (f_num Float.sub) (binop_type_err "number")
      | StarBinop ->
          eval_binary_num v1 v2 env (f_num Float.mul) (binop_type_err "number")
      | SlashBinop ->
          eval_binary_num v1 v2 env (f_num Float.div) (binop_type_err "number")
      | EqBinop -> (
          match (v1, v2) with
          | VNil, VNil -> Ok (VBool true, env)
          | VBool b1, VBool b2 -> Ok (VBool (b1 = b2), env)
          | VNum n1, VNum n2 -> Ok (VBool (n1 = n2), env)
          | VStr s1, VStr s2 -> Ok (VBool (s1 = s2), env)
          | _ -> Ok (VBool false, env))
      | NeqBinop -> (
          match (v1, v2) with
          | VNil, VNil -> Ok (VBool false, env)
          | VBool b1, VBool b2 -> Ok (VBool (b1 <> b2), env)
          | VNum n1, VNum n2 -> Ok (VBool (n1 <> n2), env)
          | VStr s1, VStr s2 -> Ok (VBool (s1 <> s2), env)
          | _ -> Ok (VBool true, env))
      | LtBinop ->
          eval_binary_num v1 v2 env
            (fun n1 n2 -> VBool (n1 < n2))
            (binop_type_err "number")
      | LeqBinop ->
          eval_binary_num v1 v2 env
            (fun n1 n2 -> VBool (n1 <= n2))
            (binop_type_err "number")
      | GtBinop ->
          eval_binary_num v1 v2 env
            (fun n1 n2 -> VBool (n1 > n2))
            (binop_type_err "number")
      | GeqBinop ->
          eval_binary_num v1 v2 env
            (fun n1 n2 -> VBool (n1 >= n2))
            (binop_type_err "number")
      | LogOrBinop | LogAndBinop ->
          raise Unreachable (* These cases are already handled *))
  | Grouping inner -> eval inner env
  | Assignment (name, expr) ->
      let+ v, env = eval expr env in
      let+ env' = Env.assign name v env in
      Ok (v, env')
  | Call (expr, args) -> (
      let rec map_args args env arg_values =
        match args with
        | List.[] -> Ok (arg_values, env)
        | List.(hd :: tl) ->
            let+ v, env = eval hd env in
            map_args tl env List.(arg_values @ [ v ])
      in
      let+ fn, env = eval expr env in
      match fn with
      | VCallable (arity, fn) ->
          let n = List.length args in
          if n <> arity then
            Error
              (Err.RuntimeError
                 (Printf.sprintf "Expected %d arguments but got %d." arity n))
          else
            let+ args, env = map_args args env List.[] in
            fn (args, env)
      | _ -> Error (Err.RuntimeError "Can only call functions and classes."))
