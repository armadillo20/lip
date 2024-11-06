type token =
  | LPAREN
  | RPAREN
  | ASSIGN
  | PLUS
  | SEQ
  | ATOK 
  | BTOK
  | DTOK
  | CTOK
  | ETOK
  | ID of string
  | CONST of string
  | EOF

let string_of_token = function
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | ASSIGN -> "ASSIGN"
  | PLUS -> "PLUS"
  | SEQ -> "SEQ"
  | ATOK -> "ATOK"
  | BTOK -> "BTOK"
  | CTOK -> "CTOK"
  | DTOK -> "DTOK"
  | ETOK -> "ETOK"
  | ID(s) -> "ID(" ^ s ^ ")"
  | CONST(s) -> "CONST(" ^ s ^ ")"
  | EOF -> "EOF"
