let hd = function
    [] -> raise (Failure "Empty list")
  | h::_ -> h;;


let tl = function
    [] -> raise (Failure "Empty list")
  | _::t -> t;;


let rec length = function
    [] -> 0
  | _::t -> 1 + length t;;


let length l =
  let rec aux n = function
      [] -> n
    | _ :: t -> aux (n + 1) t
  in aux 0 l;;
  
  
let rec compare_lengths l1 l2 =
  match (l1, l2) with
      ([], []) -> 0
    | ([], _) -> -1
    | (_, []) -> 1
    | (_::t1, _::t2) -> compare_lengths t1 t2;;


let rec compare_length_with l n = 
  if n < 0 then 1
  else 
    match (l, n) with
        ([],0) -> 0
       |([], _) -> -1
       |(_, 0) -> 1
       |(_::t, _) -> compare_length_with t (n - 1);;
  
  
let rec init n f =
  if n < 0 then raise (Invalid_argument "init")
  else
    let rec start aux =
      if aux < n then f aux :: start (aux + 1)
      else []
    in start 0;;


let rec nth l n =
  if n < 0 then raise (Invalid_argument "nth")
  else
    if l = [] then raise (Failure "nth")
    else
      if n = 0 then hd l
      else nth (tl l) (n - 1);;


let rec append l1 l2 =
  match l1 with
      [] -> l2
    | h :: t -> h :: append t l2;;
  

let rec rev_append l1 l2 =
  match l1 with
      [] -> l2
    | h :: t -> rev_append t ( h :: l2);;
    
    
let rev l = rev_append l [];;
  

let rec concat = function
    [] -> []
  | h :: t -> append h (concat t);;


let flatten = concat;;
  
  
let rec split = function
    [] -> ([], [])
  | (x, y) :: t ->
      let t1, t2 = split t in
        (x :: t1, y :: t2);;
      

let rec combine l1 l2 =
  match (l1, l2) with
      ([], []) -> []
    | ([], _) -> raise (Invalid_argument "combine")
    | (_, []) -> raise (Invalid_argument "combine")
    | ( h1 :: t1, h2 :: t2) -> (h1, h2) :: combine t1 t2;;
    

let rec map f l =
  match l with
      [] -> []
    | h :: t -> f h :: map f t;;
   
   
let rec map2 f l1 l2 =
  match (l1, l2) with
      ([], []) -> []
    | ([], _) -> raise (Invalid_argument "map2")
    | (_, []) -> raise (Invalid_argument "map2")
    | ( h1 :: t1, h2 :: t2) -> (f h1 h2) :: map2 f t1 t2;;
    
let rev_map f l =
  let rec aux x = function
    | [] -> x
    | h :: t -> aux (f h :: x) t
  in aux [] l;;

  
let rec for_all f l =
  match l with
      [] -> true
    | h :: t -> f h && for_all f t;;
        

let rec exists f l =
  match l with
      [] -> false
    | h :: t -> f h || exists f t;;
        
  
let rec mem x l =
  match l with
      [] -> false
    | h :: t -> 
        if x = h then true
        else mem x t;;
  

let rec find f = function
  | [] -> raise Not_found
  | h :: t -> if f h then h else find f t;;
  
  
let rec filter f l =
  match l with
      [] -> []
    | h :: t -> 
        if f h then h :: filter f t
        else filter f t;;
  
let find_all f l= filter f l;;
  
  
let rec partition f l =
  match l with
      [] -> ([] , [])
    | h :: t -> 
      let l1, l2 = partition f t in
        if f h then (h :: l1,  l2) 
        else (l1, h :: l2);;
        

let rec fold_left f x l =
  match l with
      [] -> x
    | h :: t -> fold_left f (f x h) t;;


let rec fold_right f l x =
  match l with
      [] -> x
    | h :: t -> f h (fold_right f t x);;
    
    
let rec assoc x = function
    [] -> raise Not_found
  | h :: t -> 
     if fst h = x then snd h
     else assoc x t;;
       
       
let rec mem_assoc x l =
  match l with
      [] -> false
    | h :: t -> 
       if fst h = x then true
       else mem_assoc x t;;


let rec remove_assoc x l =
  match l with
      [] -> []
    | h :: t ->
        if fst h = x then remove_assoc x t
        else (h :: remove_assoc x t);;







  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
