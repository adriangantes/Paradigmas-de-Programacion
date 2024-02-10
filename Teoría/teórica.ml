let v0 = function n -> 2 * n 1;; (*(int -> int) -> int*)

let v1 = function n -> n 1;; (*(int -> 'a) -> 'a*)

let v2 = function n -> (function i -> n + i);; (*int -> int -> int*)

let per_area = 
  let pi = 2. *. asin 1.0 in 
    function r -> 2. *. pi *. r,
      pi *. r *. r;;
      
let rec quo x y = (* x >= 0, y > 0 *)
  if x < y then 0
  else 1 + quo (x - y) y;;
  
let rec rem x y = (* x >= 0, y > 0 *)
  if x < y then x
  else rem (x - y) y;;
  
let div x y = (* x >= 0, y > 0 *)
  quo x y, rem x y;;
  
let rec div x y = (* x >= 0, y > 0 *)
  if x < y then 0, x
  else let (q, r) = div (x - y) y in
    1 + q, r;;


let crono f x y = 
  let t = Sys.time() in
  let _ = f x y in
  Sys.time () -. t;;


let rec length l =
  if l = [] then 0
  else 1 + length (List.tl l);;
  
  
let length ll =
  let rec aux (l, i) =
    if l = [] then i
    else aux (List.tl l, i + 1)
  in aux (ll, 0);;
  
  
let rec last l =
  if List.tl l = [] then List.hd l
  else last (List.tl l);; 
  
  
let hd = function (h :: t) -> h;;

let hd (h :: t) = h;;

let rec length = function
    [] -> 0
  | _ :: t -> 1 + length t;;
  
let rec last = function
    h :: [] -> h
  | _ :: t -> last t;;

let compare_lengths l1 l2 =
  compare(List.length l1) (List.length l2);;
  
let rec compare_lengths l1 l2 =
  match (l1, l2) with
      ([], []) -> 0
    | ([], _ -> -1
    | (_, []) -> 1
    | (_::t1, _::t2) -> compare_lengths t1 t2;;
    
    
let rec lmax = function
    [] -> raise (Failure "lmax")
  | h :: [] -> h
  | h1 :: h2 :: t -> lmax (max h1 h2 :: t);;
  
  
let rec sorted =  function
    [] | _::[] -> true
  | h1::h2::t -> h1 <= h2 && sorted (h2::t);;
  
  
let rec insert x = function
    [] -> [x]
  | h::t -> if x <= h then x::h::t
            else h::(insert x t);;
            

let rec isort = function
    [] -> []
  | h::t -> insert h (isort t);;
  
  
let rec divide = function
    [] -> [], []
  | h::[] -> [h], []
  | h1::h2::t -> let t1, t2 = divide t in
                 (h1:: t1, h2::t2);;
  
  
let rec merge = function
      [], l | l, [] -> l
    | h1::t1, h2::t2 -> if h1 <= h2 then h1::merge (t1, h2::t2)
                        else h2::merge (h1::t1, t2);;
  
  
let rec msort = function
    [] -> []
  | h::[] -> [h]
  | l -> let l1, l2 = divide l in
         merge (msort l1, msort l2);;
  

let l = [0; 1; -1; 2];;

match List.find_opt ((<) 0) l with
    None -> print_endline "valor no encontrado"
  | Some n -> print_endline (string_of_int n);;


let come (i1, j1) (i2, j2) =
  i1 = i2 || j1 = j2 || abs (i2 - i1) = abs (j2 -j1);;
  
let compatible p l =
  not (List.exists (come p) l);;
  
let queens n =
  let rec completa path i j =
    if i > n then path
    else if j > n then raise Not_found
    else if compatible (i, j) path
      then 
        try completa ((i,j)::path) (i+1) 1 with
          Not_found -> completa path i (j+1)
    else completa path i (j+1)
  in completa [] 1 1;;
  

let all_queens n =
  let rec completa path i j =
    if i > n then [path]
    else if j > n then []
    else if compatible (i, j) path
         then List.rev_append (completa ((i,j)::path) (i+1) 1) 
                              (completa path i (j+1))
         else completa path i (j+1)
  in completa [] 1 1;;


let find_opt p l =
  try Some (List.find p l) with
    Not_found -> None;;


type numero = I of int | F of float;;

let rec first_int = function
    [] -> raise Not_found
  | n :: _ -> n
  | _ :: t -> first_int t;;
  
  
type intplus = Int of int | NoInt;;

let div i1 i2 = match (i1, i2) with
    (_, Int 0) -> NoInt
  | (Int n1, Int n2) -> Int (n1 / n2)
  | _ -> NoInt;;

  
let (//) = div;;

type palo = Diamante | Corazon | Pica | Trebol;; 

let esrojo = function
    Diamante | Corazon -> true
  | _ -> false;;
  
type nat = One | Succ of nat;;

let rec nat_of_int = function
    1 -> One
  | n -> Succ (nat_of_int (n - 1));;
(*Puede entrar en el examen*)

type entero = Zero | Pos of nat | Neg of nat;;

let absoluto = function
    Neg n -> Pos n
  | e -> e;;
  
let opuesto = function
    Zero -> Zero
  | Pos n -> Neg n
  | Neg n -> Pos n;;
  
let entero_of_nat n = Pos n;;


type 'a bintree =
    Empty
  | Node of 'a * 'a bintree * 'a bintree;;
  
let rec count_nodes = function
    Empty -> 0
  | Node (_, left, right) -> 1 + count_nodes left + count_nodes right;;
  
let rec altura = function
    Empty -> -1
  | Node (_, left, right) -> 1 + max (altura left) (altura right);;
  
let rec tmax = function
    Empty -> raise (Failure)
  | Node (n, Empty, Empty) -> r
  | Node (n, Empty, tree) | Node (n, tree, Empty) -> max r (tmax tree)
  | Node (n, l, r) -> max r (max (tmax l) (tmax r));;
  
let rec preorden = function
    Empty -> []
  | Node (n, l, r) -> r :: preorden l @ preorden r;;
  
let rec hojas = function
    Empty -> []
  | Node (n, Empty, Empty) -> [n]
  | Node (n , l, r) -> hojas l @ hojas r;;
  
  
type 'a st_bintree =
    Leaf of 'a
  | N of 'a * 'a st_bintree * 'a st_bintree;;
  
  
let rec leafs = function
    Leaf x -> [x]
  | N (_, l, r) -> leafs l @ leafs r;;
  
  
type 'a tree = 
    T of 'a * 'a tree list;;
  
  
let l x = T(x, []);;


let rec nnodos (T (_, l)) = List.fold_left (+) 1 (List.map nnodos l);;


let rec nnodos = function
    T (_, []) -> 1
  | T (r, h::t) -> nnodos h + nnodos (T (r, t));;
  
  
let x = 1 and y = 2;;
let x = x + y and y =  x * y;; (* x = 3 , y = 2 *)
  
  
let x = ref 0;;
  
let next () =  x := !x + 1 ; !x;;

let reset =
  n := 0;;

let next,reset =
  let n = ref 0 in
  (fun () -> n := !n + 1; !n;;),
  (fun () -> n := 0);;
  

let fact n =
  let f = ref 1 in
  and cont = ref n in
  while !cont > 1 do
    f := !f * !cont;
    cont := !cont - 1
  done;
  !f;;

  
let fact n =
  let f = ref 1 in
  for i = 2 to n do
    f := !f * i
  done;
  !f;;
 
  
(* Array: [|1; 2; 3|]    posiciones: v.(n)    guardar: v.(n) <- x *)
  
  
type person = {name : string; age : int};;


let older person =
  {name = person.name; age = person.age + 1};;


let older p =
  {p with age = p.age + 1};;


type 'a var =  {mutable valor : 'a};;

let (!!) v = v.valor;;

let initvar x = {valor = x};;

let (<<) v x = v.valor <- x;;



(*** functores ***)

module NewCounter () : sig
  val next : unit -> int
  val reset : unit -> unit
end = struct
  let n = ref 0
  let next () =
    n := !n + 1; !n
  let reset () =
    n := 0
end;;


(*** registros ***)

type counter = {next : unit -> int;
                reset : unit -> unit};;
                
let counter =
  let n = ref 0 in
  {next = (function () -> incr n; !n);
   reset = (function () -> n := 0)};;
   
   
(*** objetos ***)

let counter = object
  val mutable n = 0
  method next = n <- n + 1 ; n
  method reset = n <- 0
end;;

let doble o = 2 * o#next;;

let counter' = object
  val mutable n = 100
  method next = n <- n + 2; n
end;;

let doble_o o = object
  val mutable n = o#next
  method next = n <- n + 3; n
end;;


class new_counter = object
  val mutable n = 0
  method next = n <- n + 1 ; n
  method reset = n <- 0
end;;



  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
