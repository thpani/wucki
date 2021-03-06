%{
  open Ast
%}

%token <string> ID
%token ASGN
%token TRUE
%token FALSE
%token NONDET
%token LBRACK
%token RBRACK
%token LPAREN
%token RPAREN
%token SEMI
%token ADD
%token SUB
%token EQ
%token LE
%token LT
%token GE
%token GT
%token <int> INT
%token EOF

%start program
%type <Ast.program> program
%%

program:
  | EOF { [] }
  | statement program { $1 :: $2 }
  ;

statement:
  | LBRACK bexpr RBRACK SEMI { Assume ($2) }
  | ID ASGN expr SEMI        { Asgn ($1, $3) }
  | ID ASGN NONDET SEMI      { Havoc ($1) }
  ;

bexpr:
  | FALSE  { False }
  | TRUE   { True }
  | expr EQ expr { Eq ($1, $3) }
  | expr LE expr { Not (Gt ($1, $3)) }
  | expr LT expr { Lt ($1, $3) }
  | expr GE expr { Not (Lt ($1, $3)) }
  | expr GT expr { Gt ($1, $3) }
  ;

atomic_expr:
  | INT { Const ($1) }
  | ID  { Id ($1) }
  | LPAREN expr RPAREN { $2 }
  ;

expr:
  | atomic_expr { $1 }
  | atomic_expr ADD atomic_expr { Add ($1, $3) }
  | atomic_expr SUB atomic_expr { Sub ($1, $3) }
  ;
