%{
  open Ast
%}

%token <string> ID
%token ASGN
%token ASSUME
%token TRUE
%token FALSE
%token NONDET
%token LPAREN
%token RPAREN
%token SEMI
%token ADD
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
  | seq_stmt { $1 }
  ;

seq_stmt:
  |                    { [] }
  | statement seq_stmt { $1 :: $2 }
  ;

statement:
  | ASSUME LPAREN bexpr RPAREN SEMI     { Assume ($3) }
  | ID ASGN expr SEMI                   { Asgn ($1, $3) }
  | ID ASGN NONDET SEMI                 { Havoc ($1) }
  ;

bexpr:
  | FALSE  { False }
  | TRUE   { True }
  | NONDET { Nondet }
  | expr EQ expr { Eq ($1, $3) }
  | expr LE expr { Not (Gt ($1, $3)) }
  | expr LT expr { Lt ($1, $3) }
  | expr GE expr { Not (Lt ($1, $3)) }
  | expr GT expr { Gt ($1, $3) }
  ;

expr:
  | INT { Const ($1) }
  | ID  { Id ($1) }
  | expr ADD expr { Add ($1, $3) }
  ;
