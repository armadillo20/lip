%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token NOT
%token AND
%token OR
%token IF
%token THEN
%token ELSE
%token ZERO
%token SUCC
%token PRED
%token ISZERO
%token EOF

%start <expr> prog

%left AND OR
%right SUCC PRED ISZERO NOT

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | ZERO {Zero}
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr;
   { If(e1, e2, e3) }
  | LPAREN; e=expr; RPAREN {e}
  |NOT;e1 = expr;{Not(e1)}
  |e1 = expr;AND;e2 = expr;{And(e1,e2)}
  |e1 = expr;OR;e2 = expr;{And(e1,e2)}
  |ISZERO;e1 = expr ; {IsZero(e1)}
  |PRED;e1 = expr ; {Pred(e1)}
  |SUCC;e1 = expr ; {Succ(e1)}

