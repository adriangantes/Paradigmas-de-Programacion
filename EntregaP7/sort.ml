
let rec insert x = function
    [] -> [x]
  | h::t -> if x <= h then x :: h :: t
            else h :: insert x t;;
            
            
let rec isort = function
    [] -> []
  | h::t -> insert h (isort t);;
  
  
let bigl = List.init 30_000_000 succ;;


let insert_t x l =
  let rec insert_aux l2 = function
      [] -> List.rev (x :: l2)
    | h :: t -> if h > x then insert_aux (h::l2) t
                else List.rev_append l2 (x::h::t)
  in insert_aux [] l;;


let isort_t l =
  let rec isort_aux l2 = function
      [] -> List.rev l2
    | h :: t -> isort_aux (insert_t h l2) t
  in isort_aux [] l;;
  
  
let rec rlist n =
  if n <= 0 then []
  else Random.int 100_000 :: rlist (n - 1);;


let crono f x =
  let t = Sys.time () in
  let _ = f x in
  Sys.time () -. t;;
  
let lc1 = List.init 10_000 succ;;
let lc2 = List.init 20_000 succ;;

let ld1 = List.init 10_000 pred;;
let ld2 = List.init 20_000 pred;;

let lr1 = rlist 10_000;;
let lr2 = rlist 20_000;; 

(*
isort lc1 = 0.00071000000000154273
isort lc2 = 0.00276400000000265322

isort ld1 = 0.00124299999999877286
isort ld2 = 0.00253000000000014325

isort lr1 = 0.559048000000000656
isort lr2 = 2.54025500000000193

isort_t lc1 = 0.00220900000000057162
isort_t lc2 = 0.00404600000000243654

isort_t ld1 = 0.00209899999999407783
isort_t ld2 = 0.00373900000000304544

isort_t lr1 = 0.786569000000000074
isort_t lr2 = 3.34745800000000315

Explicación:

Para listas ordenadas en orden descendente y ascendente tardan, tanto para
la función isort como para la isort_t, aproximadamente el doble si duplicamos
el tamaño de la lista. Sin embargo, ambas funciones tardan cerca de cinco
veces más para lista pseudoaleatorias. Esto se debe a que el algoritmo de
ordenación por inserción es menos eficiente en listas aleatorias, con una
complejidad cuadrática en el peor de los casos.

La diferencia entre isort e isort_t al aplicarlas a lr2 es, que para obtener
el resultado, isort_t usa funciones de la librería List, las cuales añaden
tiempo de ejecución. 
*)
  

let isort_g compare_fn lst =
  let insert x l =
    let rec insert_aux l2 = function
        [] -> List.rev (x :: l2)
      | h :: t -> if compare_fn x h then insert_aux (h::l2) t
                  else List.rev_append l2 (x::h::t)
    in insert_aux [] l
  in
  
  let rec isort l2 = function
      [] -> List.rev l2
    | h :: t -> isort (insert h l2) t
  in isort [] lst;;


let rec split l =
  match l with
    h1::h2::t -> let t1, t2 = split t
                 in h1::t1, h2::t2
  | _ -> l, [];;
  
  
let rec merge (l1,l2) = 
  match l1, l2 with
    [], l | l, [] -> l
  | h1::t1, h2::t2 -> if h1 <= h2 then h1 :: merge (t1, l2)
                      else h2 :: merge (l1, t2);;
                      
                      
let rec msort l =
  match l with
    [] | [_] -> l
  | _ -> let l1, l2 = split l
         in merge (msort l1, msort l2);;
         
         
let bigl2 = List.init 50_000_000 succ;;


let rec split_t l =
  let rec split_aux l1 l2 = function
  
      h1::h2::t -> split_aux (h1::l1) (h2::l2) t
    | h::[] -> List.rev (h::l1), List.rev l2
    | _ -> List.rev l1, List.rev l2
    
  in split_aux [] [] l;;
  

let merge_t (l1, l2) =
  let rec merge_aux l_aux l_aux1 l_aux2 =
  
    match (l_aux1, l_aux2) with
        [], l | l, [] -> List.rev_append l_aux l
      | h1::t1, h2::t2 ->
          if h1 <= h2 then merge_aux (h1::l_aux) t1 l_aux2
          else merge_aux (h2::l_aux) l_aux1 t2
       
  in merge_aux [] l1 l2;;
                      
                      
let rec msort' l =
  match l with
      [] | [_] -> l
    | _ -> let l1, l2 = split_t l
           in merge_t (msort' l1, msort' l2);;

  
let bigl3 = [];; 

(* 

msort' divide la lista en dos, es decir n/2 la primera
vez que se aplica, y la siguiente llamada la divide la primera mitad en
dos, y así sucesivamente, creando lo que podriamos considerar un árbol
binario de llamadas recursivas, las divisiones de la lista no se guardan
en la pila, por lo que no puede dar Stack Overflow (Creando una lista de
300_000_000 zsh interrumpe OCaml) 

*)


(*
msort lc1 = 0.00933599999999756847
msort lc2 = 0.018556000000000239

msort ld1 = 0.008632999999996116
msort ld2 = 0.0180059999999997444

msort lr1 = 0.00996100000000055275
msort lr2 = 0.0190990000000006432

msort' lc1 = 0.012437999999999505
msort' lc2 = 0.0241499999999987836

msort' ld1 = 0.0123979999999974666
msort' ld2 = 0.0235970000000023106

msort' lr1 = 0.0129990000000006489
msort' lr2 = 0.0270520000000011862

Explicación:

Para cualquier lista tarda aproximadamente el doble si duplicamos
el tamaño de la lista, ya que fusión hara el mismo número de llamadas
recursivas independientemente de si el vector esta ordenado o no.

La diferencia entre msort e msort' al aplicarlas a lr1 y lr2 es, que para
obtener el resultado, al igual que sucedia con el caso de isort e isort_t
, msort' usa funciones de la librería List, las cuales añaden tiempo de
ejecución. 

*)


let msort_g compare_fn l =
  let merge (l1, l2) =
    let rec merge_aux l_aux l_aux1 l_aux2 =
  
      match (l_aux1, l_aux2) with
          [], l | l, [] -> List.rev_append l_aux l
        | h1::t1, h2::t2 ->
            if compare_fn h1 h2 then merge_aux (h1::l_aux) t1 l_aux2
            else merge_aux (h2::l_aux) l_aux1 t2
       
    in merge_aux [] l1 l2
                                       
  in
  let rec msort l =
    match l with
        [] | [_] -> l
      | _ -> let l1, l2 = split_t l
           in merge (msort l1, msort l2)
  in msort l;;
  
