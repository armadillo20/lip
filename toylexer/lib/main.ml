open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* frequency : int -> 'a list -> ('a * int) list *)
    
    
let rec firstn n l = match n,l with 
    _,[]-> failwith("not enough elements bro")
  |0,_-> []
  |_,a::tl -> [a]@firstn (n-1) tl 
      
      
      
      
let frequency n lt = List.map(fun (a,b) -> (b,a)) (firstn n (
    List.rev( List.sort_uniq compare
                (List.map (fun x ->
                     ( List.fold_left (fun aux y -> if x = y then 1 + aux else aux) 0 lt)
                   ,x) lt))))
;;
