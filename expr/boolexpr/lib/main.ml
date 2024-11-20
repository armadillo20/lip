open Ast

let rec string_of_boolexpr = function
    True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"

(*This function first transforms the string in input
 into a stream of tokens. Then, is applies
 the lexer and the parser to transform this stream into an AST.*)
let parse (s : string) : boolExpr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


exception NoRuleApplies

let rec trace1 = function
    | If(True, e1, _) -> e1                     (* [S-IfTrue] *)
    | If(False, _, e2) -> e2                    (* [S-IfFalse] *)
    | If(e0, e1, e2) ->                           (* [S-If] *)
        let e0' = trace1 e0 in
        If(e0', e1, e2)
    | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


  let rec eval = function
  | True -> true
  | False -> false
  | If(cond, e1, e2) -> 
      if eval cond then eval e1  
      else eval e2               
