{
open Parser
}

(*his section defines named regular expressions,
 to be used later in the rules section. Here,
  we define an indentifier white, to denote
 sequences of one or more whitespaces (spaces and tabs)*)
let white = [' ' '\t']+

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "var" {VAR}
  | "const" {CONST}
  | ":=" {ASSIGN}
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "=" { EQ }
  | "<=" { LEQ }
  | "0" { ZERO }
  | "succ" { SUCC }
  | "pred" { PRED }
  | "iszero" { ISZERO }
  | eof { EOF }
