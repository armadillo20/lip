(* tokens *)
type token = A | B | X

(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)
  let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


let toklist_of_string s = List.map(fun x -> match x with
'A' -> A
|'B' -> B
|'='-> X 
| _ -> failwith("invalid input"))  (explode s)


(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)
    

let rec aux c l = match l with 
[] -> [] 
|a::tl-> if a = c then aux c tl else [a]@tl;;  

let valid l = match (aux A l) with 
    [] -> false 
  |l1 -> match  (aux X l1) with 
      [] -> false
    |l2 -> if (aux B l2) = [] then true else false;;


(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)

let win l = match ((List.fold_left(fun f x -> if x = A then 1 + f else f)   0 l)
,(List.fold_left(fun f x -> if x = B then 1 + f else f)   0 l) ) with 
|(a,b) -> if a > b then A else if a < b then B else X;;

(* val string_of_winner : token -> string *)
let string_of_winner w =  match w with 
|A -> "A"
|B -> "B"
|X -> "X"

