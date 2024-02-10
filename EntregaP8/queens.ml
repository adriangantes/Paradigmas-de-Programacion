
let come (i1, j1) (i2, j2) =
  i1 = i2 || j1 = j2 || abs (i2 - i1) = abs (j2 -j1);;
  
  
let compatible p l =
  not (List.exists (come p) l);;
  
  
let queens n =
  let rec completa path i j =
    if i > n then [path]
    else if j > n then []
    else 
      if compatible (i, j) path then
        List.rev_append (completa ((i,j)::path) (i+1) 1) (completa path i (j+1))
      else completa path i (j+1)
  in completa [] 1 1;;
  
  
let intable n (x,y) = 
  x>=1 && x<=n && y>=1 && y<=n;; 

let rec nosecomen = function 
    [] -> true
  | h::t -> compatible h t && nosecomen t;;
  
let is_queens_sol n sol = 
  List.length sol = n &&
  List.for_all (intable n) sol &&
  nosecomen sol;;
       
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
