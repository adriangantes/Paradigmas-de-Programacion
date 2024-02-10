let rec power x y =
  if y > 0 then
    x * power x (y - 1)
  else 1;;

let rec power' x y =
  if y mod 2 = 0 then
    if y = 0 then 1
    else power (x * x) (y / 2)
  else 
    x * power (x * x) (y / 2);;
    
(* power' es más eficiente que power gracias a que el número de llamadas recursivas es menor, y para números grandes es una reducción muy grande. *)

let rec powerf x y =
  if y mod 2 = 0 then
    if y = 0 then 1.0
    else powerf (x *. x) (y / 2)
  else 
    x *. powerf (x *. x) (y / 2);;
