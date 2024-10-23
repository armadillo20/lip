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
let real_num = ['-']? ['.']? num+ ['.']? num*

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
  | real_num { DTOK }
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }
