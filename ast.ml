(* {{{ type declarations *)

type id = string

type expr =
  | Const of int
  | Id of id
  | Add of expr * expr

type bexpr =
  | True
  | False
  | Nondet
  | Eq of expr * expr
  | Lt of expr * expr
  | Gt of expr * expr
  | Not of bexpr

type stmt =
  | Assume of bexpr
  | Asgn of id * expr
  | Havoc of id

type program = stmt list

(* }}} *)

(* printing functions {{{ *)

let rec pprint_expr = function
  | Const i -> string_of_int i
  | Id id -> id
  | Add (a, b) -> Printf.sprintf "(%s + %s)" (pprint_expr a) (pprint_expr b)

let rec pprint_bexpr = function
  | True  -> "true"
  | False -> "false"
  | Nondet -> "*"
  | Eq (a, b) -> Printf.sprintf "%s = %s" (pprint_expr a) (pprint_expr b)
  | Lt (a, b) -> Printf.sprintf "%s < %s" (pprint_expr a) (pprint_expr b)
  | Gt (a, b) -> Printf.sprintf "%s > %s" (pprint_expr a) (pprint_expr b)
  | Not g -> Printf.sprintf "!(%s)" (pprint_bexpr g)

let rec pprint_stmt = function
  | Assume g -> Printf.sprintf "assume(%s)" (pprint_bexpr g)
  | Asgn (a, b)  -> Printf.sprintf "%s := %s" a (pprint_expr b)
  | Havoc a -> Printf.sprintf "%s := *" a

and pprint_seq ?(sep=";\n") stmts =
  String.concat sep (List.map pprint_stmt stmts)
