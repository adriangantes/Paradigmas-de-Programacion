let rec factorial = function 0 -> 1 | n -> n*factorial (n-1);;

let rec sumto = function 0 -> 0 | n -> n + sumto (n-1);;

let rec exp10 n = if n >= 0 then  10 * exp10 (n-1) else 1;;
let rec exp10 = function 0 -> 1 | n -> 10 * exp10 (n-1);;

let rec num_cifras n =
  if n < 0 then
    num_cifras (-n)
  else if n < 10 then
    1  
  else
    1 + num_cifras (n / 10);;
    
let rec sum_cifras n = 
  if n < 0 then
    sum_cifras (-n)
  else if n < 10 then
    n
  else
    n mod 10 + sum_cifras (n / 10);;
