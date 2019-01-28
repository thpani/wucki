open Z3

let h = Hashtbl.create 10

(** [mk_const ctx id] makes an integer constant of name [id]. *)
let mk_const ctx id = Arithmetic.Integer.mk_const_s ctx id

(** [mk_const' ctx num_primes id] makes an integer constant with name [id__PREPRIME__(num_primes)]. *)
let mk_const' ctx num_primes id =
  let id' = id ^ "__PREPRIME__" ^ (string_of_int num_primes) in
  (* let id' = id ^ (String.make num_primes '\'') in *)
  mk_const ctx id'

(** [mk_value_const' ctx num_primes id] makes an integer constant with name [id__VALUE__(num_primes)]. *)
let mk_value_const' ctx num_primes id =
  let id' = id ^ "__VALUE__" ^ (string_of_int num_primes) in
  (* let id' = id ^ "__VALUE" ^ (String.make num_primes '\'') in *)
  mk_const ctx id'

let rec formula_of_expr ctx = let open Ast in function
  | Const i -> Arithmetic.Integer.mk_numeral_i ctx i
  | Id id -> mk_const ctx id
  | Add (a,b) -> Arithmetic.mk_add ctx
    [formula_of_expr ctx a; formula_of_expr ctx b]
  | Sub (a,b) -> Arithmetic.mk_sub ctx
    [formula_of_expr ctx a; formula_of_expr ctx b]

let rec formula_of_bexpr ctx = let open Ast in function
  | True -> Boolean.mk_true ctx
  | False -> Boolean.mk_false ctx
  | Eq (a,b) -> Boolean.mk_eq ctx (formula_of_expr ctx a) (formula_of_expr ctx b)
  | Lt (a,b) -> Arithmetic.mk_lt ctx (formula_of_expr ctx a) (formula_of_expr ctx b)
  | Gt (a,b) -> Arithmetic.mk_gt ctx (formula_of_expr ctx a) (formula_of_expr ctx b)
  | Not a -> Boolean.mk_not ctx (formula_of_bexpr ctx a)

let strongest_post ctx assertion line_no = let open Ast in function
  | Assume bexpr -> Boolean.mk_and ctx [ assertion ; formula_of_bexpr ctx bexpr ]
  | Asgn (id, expr) ->
      let unprimed_id = mk_const ctx id in
      let preprimed_id = mk_const' ctx line_no id in
      let substituted_assertion = Expr.substitute_one assertion unprimed_id preprimed_id in
      let substituted_expression = Expr.substitute_one (formula_of_expr ctx expr) unprimed_id preprimed_id in
      let body = Boolean.mk_and ctx [ Boolean.mk_eq ctx unprimed_id
        substituted_expression ; substituted_assertion ] in
      if Hashtbl.mem h id then
        let quantifier = Quantifier.mk_exists_const ctx [preprimed_id] body None [] [] None None in
        Quantifier.expr_of_quantifier quantifier
      else begin
        Hashtbl.add h id preprimed_id;
        body
      end
  | Havoc id ->
      let unprimed_id = Arithmetic.Integer.mk_const_s ctx id in
      let preprimed_id = mk_const' ctx line_no id in
      let value = mk_value_const' ctx line_no id in
      let substituted_assertion = Expr.substitute_one assertion unprimed_id preprimed_id in
      let body = Boolean.mk_and ctx [ Boolean.mk_eq ctx unprimed_id value ; substituted_assertion ] in
      if Hashtbl.mem h id then
        let quantifier = Quantifier.mk_exists_const ctx [preprimed_id; value] body None [] [] None None in
        Quantifier.expr_of_quantifier quantifier
      else begin
        Hashtbl.add h id preprimed_id;
        let quantifier = Quantifier.mk_exists_const ctx [value] body None [] [] None None in
        Quantifier.expr_of_quantifier quantifier
      end

let strongest_post_seq ctx seq assertion =
  List.fold_left (fun (assertion, line_no) stmt ->
    strongest_post ctx assertion line_no stmt, line_no+1
  ) (assertion,0) seq

let replace_names ctx assertion =
  let ids = Hashtbl.fold (fun id _ l -> id :: l) h [] in
  let primed_ids = List.map (fun id -> id ^ "'") ids in
  let prepr = Hashtbl.fold (fun _ prepr l -> prepr :: l) h [] in
  let a = assertion in
  let a = Expr.substitute a (List.map (mk_const ctx) ids) (List.map (mk_const ctx) primed_ids) in
  let a = Expr.substitute a prepr (List.map (mk_const ctx) ids) in
  a
