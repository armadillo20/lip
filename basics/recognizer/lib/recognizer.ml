let rec lang1 l1 = match l1 with 
[] -> false
|[a] -> if (a = '0' || a = '1' ) then true else false
|a::tl ->  if (a = '0' || a = '1') then lang1 tl else false
 

let rec lang2 l2 = match l2 with 
[] -> true 
|'1'::tl -> lang2 tl
|'0'::tl -> List.for_all(fun x -> x <> '0') tl 
|_ -> false

let lang3 l3 = if (List.length l3  <> 1) then  match (l3,List.rev l3) with 
    |('0'::tl,'0'::_) -> List.for_all(fun x -> x = '0' || x = '1') tl 
    |_ -> false
  else false;; 

let lang4 l4 = List.for_all(fun x -> x = '0' || x = '1') l4
                && (List.fold_left(fun acc x -> if x ='1' then 1 + acc else acc)  0  l4) = 2;;

let rec lang5 l5 =  match l5 with 
    [] -> false
  |['0';'0'] -> true
  |['1';'1'] -> true
  |'0'::'0'::tl -> lang5 tl
  |'1'::'1'::tl -> lang5 tl
  |_ -> false;;

    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
