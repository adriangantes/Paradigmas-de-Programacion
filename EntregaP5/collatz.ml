let f n = 
  if n mod 2 = 0 then n / 2 
  else 3 * n + 1;;
  
  
let rec orbit n =
  if n = 1 then "1"
  else 
    string_of_int n ^ " " ^ (orbit (f n));;
    
    
let rec length n =
  if n > 0 then
    if f n = 1 then 1
    else 1 + length (f (n))
  else 0;;
  
  
let rec top n =
  if n = 1 then 1
  else
    let next = top (f n) in
    max n next;;
    

let rec length'n'top n =
  if n > 0 then
    if n = 1 then 0, 1
    else
      let next = length'n'top (f n) in
      1 + fst next, max n (snd next)
  else 0, 0;;
  

let rec longest_in m n =
  if m > n then
    0, 0
  else
    let len = length m in
    let next = longest_in (m + 1) n in
    if len >= snd next then
      m, len
    else
      next;;


let rec highest_in m n =
  if m > n then
    0, 0 
  else
    let max = top m in
    let next = highest_in (m + 1) n in
    if max >= snd next then
      m, max
    else
      next;;

  
  
  
  
  
  
  
