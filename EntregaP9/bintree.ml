
type 'a bintree =
    Empty
  | Node of 'a * 'a bintree * 'a bintree;;
  

let rec in_order = function
    Empty -> []
  | Node (n, l, r) -> in_order l @ (n :: in_order r);;
  

let rec insert f tree x =
  match tree with
      Empty -> Node (x, Empty, Empty)
    | Node (n, l, r) -> if f x n then Node (n, insert f l x, r)
                        else Node (n, l, insert f r x);;
                          
                          
let bst f l =
  let rec aux l tree = 
    match l with
        [] -> tree
      | (h::t) -> aux t (insert f tree h)
  in aux l Empty;;


let qsort f l = 
  in_order (bst f l);;
