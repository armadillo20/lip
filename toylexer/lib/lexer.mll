{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let uppercase = ['A'-'Z']['a'-'z' '0'-'9']*
let vowel = ['a' 'e' 'i' 'o' 'u']
let lwrvowel = vowel+
let onevowel = chr* vowel chr*
let real_num = ['-' '.']+ num+ ['.']? num*
let hex_prefix = ['0'] ['x' 'X']
let hex_digit = ['0'-'9' 'A'-'F' 'a'-'f']

rule read_token = 
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ } 
  | uppercase { ATOK }
  | lwrvowel { BTOK }
  | onevowel { CTOK }
  | real_num { DTOK }
  | hex_prefix hex_digit+ { ETOK }
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }
