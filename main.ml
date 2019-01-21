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
  | Parsing.Parse_error ->
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
      print_endline (Ast.pprint_seq prog)
    end
  | _ -> prerr_endline usage ; exit 1

let () = main ()
