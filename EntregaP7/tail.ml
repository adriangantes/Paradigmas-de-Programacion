

let to0from n =
  let rec aux l n =
    if n < 0 then List.rev l
    else aux (n :: l) (n-1)
  in aux [] n;;


let fromto m n =
  let rec aux l m n =
    if m > n then List.rev l
    else aux (m :: l) (m+1) n
  in aux [] m n;;
  
  
let remove x l =
  let rec aux l_aux = function
      [] -> List.rev l_aux
    | h::t ->
       if x = h then List.rev_append l_aux t
       else aux (h :: l_aux) t
  in aux [] l;;
  

let compress l =
  let rec aux l_aux = function
      h1 :: h2 :: t ->
        if h1 = h2 then aux l_aux (h2 :: t)
        else aux (h1 :: l_aux) (h2 :: t)
    | l -> List.rev_append l_aux l
  in aux [] l;;


let append' l1 l2 = List.rev_append (List.rev l1) l2;;


let map' f l = List.rev (List.rev_map f l);;


let fold_right' f l x =
  let rec aux total = function
      [] -> total
    | h :: t -> aux (f h total) t
  in aux x (List.rev l);;

   
let incseg l =
fold_right' (fun x t -> x::map' ((+) x) t) l [];;



