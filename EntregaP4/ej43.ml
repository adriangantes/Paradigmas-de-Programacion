(*let rec reverse n =
  let rec aux_reverse n cont =
    if n = 0 then cont
    else
      let resto = n mod 10 in
      let new_cont = (cont * 10) + resto in
      aux_reverse (n / 10) new_cont
  in
  aux_reverse n 0;;*)
  

let rec reverse n =
  if n < 10 then n
  else
    let rec num_cifras n =
      if n < 0 then
        num_cifras (-n)
      else if n < 10 then
        1  
      else
        1 + num_cifras (n / 10) in
      
    let resto = n mod 10 in
    let numero = n / 10 in 
  
    resto * int_of_float(10. ** float_of_int(num_cifras numero)) + reverse (numero);;


let rec palindromo s = 
  if String.length s < 2 then true
  else if s.[0] = s.[String.length s-1] then 
    palindromo (String.sub s 1 (String.length s - 2))
  else false;;


let rec mcd (x, y) =
  if (x <> 0 || y <> 0) then
  
    if x = 0 then y
    else if y = 0 then x
    else mcd (y, (x mod y))
  
  else if (x = 0 && y = 0) then 0
  else 1;;
