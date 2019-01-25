open Z3

let mk_const' ctx id =
  let id' = id ^ "\'" in
  Arithmetic.Integer.mk_const_s ctx id'

let rec formula_of_expr ctx = let open Ast in function
  | Const i -> Arithmetic.Integer.mk_numeral_i ctx i
  | Id id -> Arithmetic.Integer.mk_const_s ctx id
  | Add (a,b) -> Arithmetic.mk_add ctx
    [formula_of_expr ctx a; formula_of_expr ctx b]

let rec formula_of_bexpr ctx = let open Ast in function
  | True -> Boolean.mk_true ctx
  | False -> Boolean.mk_false ctx
  | Eq (a,b) -> Boolean.mk_eq ctx (formula_of_expr ctx a) (formula_of_expr ctx b)
  | Lt (a,b) -> Arithmetic.mk_lt ctx (formula_of_expr ctx a) (formula_of_expr ctx b)
  | Gt (a,b) -> Arithmetic.mk_gt ctx (formula_of_expr ctx a) (formula_of_expr ctx b)
  | Not a -> Boolean.mk_not ctx (formula_of_bexpr ctx a)

let strongest_post ctx assertion = let open Ast in function
  | Assume bexpr -> Boolean.mk_and ctx [ assertion ; formula_of_bexpr ctx bexpr ]
  | Asgn (id, expr) ->
      let unprimed_id = Arithmetic.Integer.mk_const_s ctx id in
      let preprimed_id = mk_const' ctx id in
      let substituted_assertion = Expr.substitute_one assertion unprimed_id preprimed_id in
      let substituted_expression = Expr.substitute_one (formula_of_expr ctx expr) unprimed_id preprimed_id in
      let body = Boolean.mk_and ctx [ Boolean.mk_eq ctx unprimed_id
        substituted_expression ; substituted_assertion ] in
      let quantifier = Quantifier.mk_exists_const ctx [preprimed_id] body None [] [] None None in
      Quantifier.expr_of_quantifier quantifier
  | Havoc id ->
      let unprimed_id = Arithmetic.Integer.mk_const_s ctx id in
      let preprimed_id = mk_const' ctx id in
      let value = Arithmetic.Integer.mk_const_s ctx (id ^ "_VALUE") in
      let substituted_assertion = Expr.substitute_one assertion unprimed_id preprimed_id in
      let body = Boolean.mk_and ctx [ Boolean.mk_eq ctx unprimed_id value ; substituted_assertion ] in
      let quantifier = Quantifier.mk_exists_const ctx [preprimed_id] body None [] [] None None in
      Quantifier.expr_of_quantifier quantifier

let strongest_post_seq ctx seq assertion =
  List.fold_left (strongest_post ctx) assertion seq
