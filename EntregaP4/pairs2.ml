let next (x, y) =
  if (x + y) mod 2 = 1 then 
    if y = 1 then (x + 1, y)
    else (x + 1, y - 1)
  else 
    if x = 1 then (x, y + 1)
    else (x - 1, y + 1);;
     
     
let rec steps_from (x, y) n =
  if n <= 0 then (x, y)
  else steps_from (next(x, y)) (n - 1);;
  
  
let pair n =
  if n <= 0 then (0, 0)
  else steps_from (1, 1) (n - 1);;


(*

let pair_i p =
  let rec find i =
    if pair i = p then i
    else find (i+1)
  in find 1;;


let crono f x = 
  let t = Sys.time() in
  let _ = f x in
  Sys.time () -. t;;
  
*)
  
  
let pair_i' (x, y) =
  if (x + y) mod 2 = 1 then 
    let (x', y') = steps_from (x, y) (y - 1) in
      (x' / 2) * (y' + x') - (y - 1)
  else 
    let (x', y') = steps_from (x, y) (x - 1) in
      (y' / 2 + 1) * y' - (x - 1) ;;
    
    
(* La anterior implementación era tan lenta debido a que empezaba en la
   primera posición (1, 1) y recorría toda las posiciones de una en una
   hasta llegar a la buscada. La nueva implementación busca el mayor
   elemento de su fila, ya sea (x, 1) o (1, y), calcula cual es el valor
   de dicha posición y le resta la longitud a la que se encuentra el
   elemento que buscamos, x o y. Depende de que la suma de sus coordenadas
   sea par o impar. *)
    
    
