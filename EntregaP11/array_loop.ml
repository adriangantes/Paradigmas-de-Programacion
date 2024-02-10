

let append ar1 ar2 =

  let l1 = Array.length ar1 in
  let l2 = Array.length ar2 in
  
  Array.init (l1 + l2) 
    (fun i ->
              if i < l1 then ar1.(i)
              else ar2.(i - l1));;



let sub ar ini cont =

  if ini < 0 || cont < 0 || cont + ini > Array.length ar then 
    raise (Invalid_argument "sub")
  else Array.init (cont) (fun i -> ar.(i + ini));;



let copy ar = Array.init (Array.length ar) (fun i -> ar.(i));;



let fill ar ini cont valor =

  if ini < 0 || cont < 0 || cont + ini > Array.length ar then 
    raise (Invalid_argument "fill")
  else 
  
    let i = ref ini in
    
    while !i < cont + 1 do
      ar.(!i) <- valor;
      i := !i + 1
    done;;
   


let blit ar ini dest pos cont =
  
  let x = pos + cont in

  if ini < 0 || cont < 0 || pos < 0 || x > Array.length dest
     || (ini + cont) > Array.length ar then raise (Invalid_argument "blit")
  else 
  
    let i = ref pos in
    let j = ref ini in
    
    while !i < x do
      dest.(!i) <- ar.(!j);
      i := !i + 1;
      j := !j + 1
    done;;



let to_list ar =

  let i = ref (Array.length ar) in
  let sol = ref [] in
  
  while !i > 0 do
    sol := ar.(!i - 1) :: !sol;
    i := !i - 1
  done;
  
  !sol;;



let of_list l = Array.init (List.length l) (fun x -> List.nth l x);;
  
  

let iter (f : 'a -> unit) ar =

  if ar = [||] then ()
  else
  
    let i = ref 0 in
    let len = Array.length ar in
    
    while !i < len do
      f (ar.(!i));
      i := !i + 1
    done;;



let fold_left f r ar =
  
  let result = ref r in
  let i = ref 0 in
  let len = Array.length ar in
  
  while !i < len do
    result := f !result (ar.(!i));
    i := !i + 1
  done;
  
  !result;;



let for_all f ar =

  let i = ref 0 in
  let result = ref true in
  let len = Array.length ar in
  
  while !i < len do
  
    if f (ar.(!i)) then
      i := !i + 1
    else
      begin
        i := len;
        result := false
      end
      
  done;
  
  !result;;



let exists f ar =

  let i = ref 0 in
  let result = ref false in
  let len = Array.length ar in
  
 
  while !i < len do
  
    if f (ar.(!i)) then
      begin
        i := len;
        result := true
      end
    else
      i := !i + 1
      
  done;
  
  !result;;



let find_opt f ar =
  
  let i = ref 0 in
  let result = ref false in
  let len = Array.length ar in
  let value = ref None in 
  
  while !i < len do
  
    if f (ar.(!i)) then
      begin
        result := true;
        value := Some ar.(!i);
        i := len
      end
    else
      i := !i + 1
      
  done;
  
  !value;;
  










