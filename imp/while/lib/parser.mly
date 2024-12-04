%{
open Ast
%}

%token TRUE
%token FALSE
%token VAR
%token CONST
%token LPAREN
%token RPAREN
%token NOT
%token AND
%token OR
%token ADD
%token SUB
%token MUL
%token EQ
%token LEQ
%token THEN
%token ELSE
%token ZERO
%token SUCC
%token PRED
%token ISZERO
%token EOF


%token SKIP
%token ASSIGN
%token SEQ
%token IF
%token WHILE

%start <prog> prog


%left AND OR
%right SUCC PRED ISZERO NOT

%%

prog:
  | e = expr; EOF { e }
  | c = cmd; EOF { c }
  
;

expr:
  | TRUE { True }
  | FALSE { False }
  | v = string {Var(v)}
  | c = int {Const(c)}
  | CONST ;LPAREN ;v = int ; RPAREN {Const(v)}
  | ZERO {Zero}
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr;
   { If(e1, e2, e3) }
  | LPAREN; e=expr; RPAREN {e}
  |NOT;e1 = expr;{Not(e1)}
  |e1 = expr;AND;e2 = expr;{And(e1,e2)}
  |e1 = expr;OR;e2 = expr;{And(e1,e2)}
  |e1 = expr ; ADD ; e2 = expr ; {Add(e1, e2)}
  |e1 = expr ; SUB ; e2 = expr ; {Sub(e1, e2)}
  |e1 = expr ; MUL ; e2 = expr ; {Mul(e1, e2)}
  |e1 = expr ; EQ ; e2 = expr ; {Eq(e1, e2)}
  |e1 = expr ; LEQ ; e2 = expr ; {Leq(e1, e2)}
  |ISZERO;e1 = expr ; {IsZero(e1)}
  |PRED;e1 = expr ; {Pred(e1)}
  |SUCC;e1 = expr ; {Succ(e1)}


  cmd :
  |SKIP {Skip}

