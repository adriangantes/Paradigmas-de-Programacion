let is_prime n =
  let rec check_from i =
    i >= n ||
    (n mod i <> 0 && check_from (i+1))
  in check_from 2;;

let rec next_prime n = 
  if is_prime (n + 1) then (n + 1)
  else next_prime (n + 1);;
  
let rec last_prime_to n =
  if is_prime n then n
  else last_prime_to (n - 1);;
  
let is_prime2 n =
  if n < 1 then false
  else 
    if n <= 3 then true
    else 
      if n mod 2 = 0 || n mod 3 = 0 || n mod 5 = 0 then false
      else
        let rec es_primo_aux divisor =
          if divisor * divisor > n then true
          else 
            if n mod divisor = 0 then false
            else es_primo_aux (divisor + 2)
        in es_primo_aux 7;;
