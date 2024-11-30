open Ast
type exprval = Bool of bool | Nat of int
type exprtype = BoolT | NatT


let string_of_type = function 
|BoolT -> "BoolT"
|NatT -> "NatT"



 (*funzione typechec da implementare expr -> exprtype*)
(*type expr =
    True
  | False
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | If of expr * expr * expr
  | Zero
  | Succ of expr
  | Pred of expr
  | IsZero of expr

  errore nel caso -> exception TypeError of string;;*)

  exception TypeError of string

  let rec typecheck = function
  | True -> BoolT
  | False -> BoolT
  | Zero -> NatT
  | Not e1 -> 
      (match typecheck e1 with
       | BoolT -> BoolT
       | _ -> raise (TypeError "Not requires a Bool"))
  | And (e1, e2) | Or (e1, e2) -> 
      (match typecheck e1, typecheck e2 with
       | BoolT, BoolT -> BoolT
       | _ -> raise (TypeError "And/Or require two Bools"))
  | If (e0, e1, e2) -> 
      (match typecheck e0, typecheck e1, typecheck e2 with
       | BoolT, BoolT, BoolT  -> BoolT
       | _, _, _ -> raise (TypeError "Condition of If must be a Bool")
        )
  | Succ e1 | Pred e1 -> 
      (match typecheck e1 with
       | NatT -> NatT
       | _ -> raise (TypeError "Succ/Pred require a Nat"))
  | IsZero e1 -> 
      (match typecheck e1 with
       | NatT -> BoolT
       | _ -> raise (TypeError "IsZero requires a Nat"))


let rec string_of_expr = function 
| True -> "True"
| False -> "False"
| If (e0, e1, e2) ->
    "If(" ^ string_of_expr e0 ^ "," ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"
| Not e1 ->
    "Not(" ^ string_of_expr e1 ^ ")"
| And (e1, e2) ->
    "And(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"
| Or (e1, e2) ->
    "Or(" ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ ")"
| Zero ->
    "Zero"
| Succ e1 ->
    "Succ(" ^ string_of_expr e1 ^ ")"
| Pred e1 ->
    "Pred(" ^ string_of_expr e1 ^ ")"
| IsZero e1 ->
    "IsZero(" ^ string_of_expr e1 ^ ")"



(*This function first transforms the string in input
 into a stream of tokens. Then, is applies
 the lexer and the parser to transform this stream into an AST.*)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


exception NoRuleApplies

let rec trace1 = function
  | If(True, e1, _) -> e1                     (* [S-IfTrue] *)
  | If(False, _, e2) -> e2                    (* [S-IfFalse] *)
  | If(e0, e1, e2) ->                         (* [S-If] *)
      let e0' = trace1 e0 in
      If(e0', e1, e2)
  | Not(True) -> False                        (* [S-NotTrue] *)
  | Not(False) -> True                        (* [S-NotFalse] *)
  | Not(e) ->                                 (* [S-Not] *)
      let e' = trace1 e in
      Not(e')
  | And(True, e2) -> e2   
  | And(e2,True ) -> e2                      (* [S-AndTrue] *)
  | And(False, _) -> False    
  | And(_,False ) -> False                (* [S-AndFalse] *)
  | And(e1, e2) ->                           (* [S-And] *)
    let e1' = trace1 e1 in
    And(e1', e2)
  | Or(True, _) -> True                       (* [S-OrTrue] *)
  | Or(False, e2) -> e2                       (* [S-OrFalse] *)
  | Or(e1, e2) ->                             (* [S-Or] *)
      let e1' = trace1 e1 in
      Or(e1', e2)
  | Succ(e) ->                                (* [S-Succ] *)
      let e' = trace1 e in
      Succ(e')                       (* [S-PredZero] *)
  | Pred(Succ(e)) -> e                        (* [S-PredSucc] *)
  | Pred(e) ->                                (* [S-Pred] *)
      let e' = trace1 e in
      Pred(e')
  | IsZero(Zero) -> True                      (* [S-IsZeroZero] *)
  | IsZero(Succ(_)) -> False                  (* [S-IsZeroSucc] *)
  | IsZero(e) ->                              (* [S-IsZero] *)
      let e' = trace1 e in
      IsZero(e')
  | _ -> raise NoRuleApplies                  (* Nessuna regola applicabile *)


let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


  let rec eval = function
  | True -> Bool true
  | False -> Bool false
  | Zero -> Nat 0
  | If (cond, e1, e2) ->
      (match eval cond with
       | Bool true -> eval e1
       | Bool false -> eval e2
       | _ -> failwith "If condition must evaluate to Bool")
  | Not e1 ->
      (match eval e1 with
       | Bool b -> Bool (not b)
       | _ -> failwith "Not requires a Bool")
  | And (e1, e2) ->
      (match eval e1, eval e2 with
       | Bool b1, Bool b2 -> Bool (b1 && b2)
       | _ -> failwith "And requires two Bools")
  | Or (e1, e2) ->
      (match eval e1, eval e2 with
       | Bool b1, Bool b2 -> Bool (b1 || b2)
       | _ -> failwith "Or requires two Bools")
  | Succ e1 ->
      (match eval e1 with
       | Nat n when n >= 0 -> Nat (n + 1)
       | _ -> failwith "Succ requires a Nat")


  | IsZero e1 ->
      (match eval e1 with
       | Nat 0 -> Bool true
       | Nat _ -> Bool false
       | _ -> failwith "IsZero requires a Nat")

       | Pred e1 ->
        (match eval e1 with
         | Nat n when n > 0 -> Nat(n-1)
         | _ -> failwith("errore nessun numero prima di 0"))

