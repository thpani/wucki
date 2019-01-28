(** Return the current position in [lexbuf] as string. *)
let position_string lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.sprintf "%d:%d" pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

(** Parse the program file at [path]. *)
let parse_program path =
  let input = open_in path in
  let lexbuf = Lexing.from_channel input in
  try
    let parsed = Parser.program Lexer.token lexbuf in
    close_in input ; parsed
  with
  | Lexer.Error msg ->
    Printf.eprintf "[Lexer error] %s @  %s: %s\n" path (position_string lexbuf) msg ;
    exit (1)
  | Parser.Error ->
    Printf.eprintf "[Parser error] %s @ %s\n" path (position_string lexbuf) ;
    exit (1)

let main () =
  let args = ref [] in
  let usage = "Usage: wucki <prog>" in
  Arg.parse [] (fun s -> args := s :: !args) usage
  ;

  match !args with
  | [ fn_prog ] ->
    begin
      let prog = parse_program fn_prog in
      (* print_endline (Ast.pprint_seq prog); *)
      let ctx = Z3.mk_context [] in
      let assertion = ref (Z3.Boolean.mk_true ctx) in
      print_endline ("{ " ^ (Z3.Expr.to_string !assertion) ^ " }");
      List.iteri (fun line_no stmt ->
        (* stmt *)
        print_endline (Ast.pprint_stmt stmt) ;

        (* strongest post *)
        assertion := Solver.strongest_post ctx !assertion line_no stmt ;
        (* print_endline ("{ " ^ (Z3.Expr.to_string !assertion) ^ " }"); *)
        assertion := Z3.Expr.simplify !assertion None;
        print_endline ("{ " ^ (Z3.Expr.to_string !assertion) ^ " }");
      ) prog;

        (* qe strongest post *)
        let goal = Z3.Goal.mk_goal ctx false false false in
        Z3.Goal.add goal [ !assertion ];
        let ar = Z3.Tactic.apply (Z3.Tactic.mk_tactic ctx "qe") goal None in
        assert ((Z3.Tactic.ApplyResult.get_num_subgoals ar) = 1) ;
        let expr = Z3.Goal.as_expr (Z3.Tactic.ApplyResult.get_subgoal ar 0) in
        assertion := Z3.Expr.simplify expr None;
        assertion := Solver.replace_names ctx !assertion;
        print_endline ("{ " ^ (Z3.Expr.to_string !assertion) ^ " }");

        (* check sat *)
        let s = Z3.Solver.mk_solver ctx None in
        Z3.Solver.add s [ !assertion ] ;
        let result = Z3.Solver.check s [] in
        print_endline (Z3.Solver.string_of_status result)
    end
  | _ -> prerr_endline usage ; exit 1

let () = main ()
