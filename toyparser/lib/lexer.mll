{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let hex = "0" ['x''X'] ['0'-'9''A'-'F''a'-'f']+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MUL }
  | "/" { DIV }

  | num { CONST (Lexing.lexeme lexbuf) }
  | hex { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
