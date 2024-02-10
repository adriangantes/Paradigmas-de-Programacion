let v = abs(-5);;

let w = 10.0 +. 5.0 *. 2.0 -. 3.0 /. 1.5;;

let x = if (int_of_float(w) < v) then 'T' else 'F';;

let y = (int_of_float(4. *. w) < 2*v);;

let z = "El valor es: " ^ (string_of_int v);;
