

let length l =

  let t = ref l in
  let cont = ref 0 in
  
  while !t <> [] do
    cont := !cont + 1;
    t := List.tl !t
  done;
  
  !cont;;
  
  
  
let last l =

  if l = [] then
    failwith "last"
  else
  
    let t = ref (List.tl l) in
    let value = ref (List.hd l) in
    
    while !t <> [] do
      value := List.hd !t;
      t := List.tl !t
    done;
    
  !value;;
     
      

let nth l pos =
  
  if pos < 0 then raise (Invalid_argument "nth")
  else
    let t = ref l in
    let i = ref 0 in
  
    while !t <> [] && !i <> pos do
      t := List.tl !t;
      i := !i + 1
    done;
  
    if !t = [] then
      raise (Failure "nth")
    else
      List.hd !t;;



let rev l =

  let aux = ref [] in
  let t = ref l in
  
  while !t <> [] do
    aux := List.hd !t :: !aux;
    t := List.tl !t
  done;
  
  !aux;;
  
  

let append l1 l2 =

  let aux1 = ref (rev l1) in
  let aux2 = ref l2 in
  
  while !aux1 <> [] do
    aux2 := List.hd !aux1 :: !aux2;
    aux1 := List.tl !aux1
  done;
  
  !aux2;;



let concat l =

  let aux = ref [] in
  let aux2 = ref [] in
  let t = ref l in
  
  while !t <> [] do
    aux2 := List.hd !t;
    
    while !aux2 <> [] do
      aux := List.hd !aux2 :: !aux;
      aux2 := List.tl !aux2
    done;
    
    t := List.tl !t;
  done;
  
  rev !aux;;



let for_all f l =

  let t = ref l in
  let result = ref true in
  
  while !t <> [] do
  
    if f (List.hd !t) then
      t := List.tl !t
    else
      begin
        t := [];
        result := false
      end
      
  done;
  
  !result;;
   


let exists f l =

  let t = ref l in
  let result = ref false in
  
  while !t <> [] do
  
    if f (List.hd !t) then   
      begin
        t := [];
        result := true
      end
    else
      t := List.tl !t
      
  done;
  
  !result;;



let find_opt f l =
  
  let t = ref l in
  let result = ref false in
  let value = ref None in 
  
  while !t <> [] do
    let h = List.hd !t in
  
    if f h then   
      begin
        t := [];
        result := true;
        value := Some h
      end
    else
      t := List.tl !t
      
  done;
  
  !value;;
  


let iter (f : 'a -> unit) l =

  let t = ref l in
  
  while !t <> [] do
    f (List.hd !t);
    t := List.tl !t
  done;;



let fold_left f r l =
  
  let result = ref r in
  let t = ref l in
  
  while !t <> [] do
    result := f !result (List.hd !t) ;
    t := List.tl !t
  done;
  
  !result;;










