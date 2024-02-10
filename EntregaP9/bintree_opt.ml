open Bintree

let is_bst f tree =
  let l = in_order tree in
  let rec aux l = 
    match l with
      [] -> true
    | (h1::[]) -> true
    | (h1::h2::t) -> f h1 h2 && aux (h2::t)
  in aux l;; 
  
  
let bfs tree =
  let rec aux queue acc =
    match queue with
        [] -> List.rev acc
      | Empty :: t -> aux t acc
      | Node (value, left, right) :: t ->
          aux (t @ [left; right]) (value :: acc)
  in aux [tree] [];;
  
  
let bfs' tree =
  let rec aux queue acc =
    match queue with
        [] -> List.rev acc
      | Empty :: t -> aux t acc
      | Node (value, left, right) :: t ->
          aux (List.append t [left; right]) (value :: acc)
  in aux [tree] [];;


let rec height = function
  | Empty -> 0
  | Node (_, left, right) -> 1 + max (height left) (height right);;
  

let rec count_nodes = function
    Empty -> 0
  | Node (_, left, right) -> 1 + count_nodes left + count_nodes right;;
  

let rec pow2 n = 
    if n = 0 then 1
    else if n mod 2 = 0 then let y = pow2 (n/2) in y * y
    else 2 * pow2 (n-1);;
    

let perfecto tree = (count_nodes tree = (pow2 (height tree) - 1));;
  
  
let casi_completo tree = 
  let rec aux = function
      Empty -> true
    | Node (_, Empty, Empty) -> true
    | Node (_, Empty, right) -> false
    | Node (_, left, right) -> 
        let hl = height left in
        let hr = height right in 
        
        if perfecto left && perfecto right && (hl = hr) then true
        else
          if perfecto right then hl - 1 = hr && aux left
          else 
            if perfecto left then hl = hr && aux right
            else false
  in aux tree;;





