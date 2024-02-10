let p radio = 2.0 *. 3.141592 *. radio;;

let area radio = 3.141592 *. radio *. radio;;

let absf x = if x >= 0.0 then x else -.x;;

let even x = x mod 2 = 0;;

let next3 x = x + (3 - x mod 3) mod 3;;
let next3 x =
	if (x mod 3) = 0 then x 
	else x + (3 - (x mod 3));;
	
	
let is_a_letter c = (c>='a' && c<='z') || (c>='A' && c<='Z');;

let string_of_bool condicion = if condicion then "verdadero" else "falso";;
