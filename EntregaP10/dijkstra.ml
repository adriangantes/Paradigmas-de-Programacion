exception Invalid_argument of string

let dijkstra m =

  let n = Array.length m in
  
  if Array.for_all( fun r -> Array.length r = n) m then
    if n = 0 then
      [||]
    else
      raise (Invalid_argument "dijkstra: la matriz no es cuadrada")
    
  else
  
    let distancias = Array.copy m in
    
    for i = 0 to n in
      for j = 0 to n in
        if (m.(i).(j) <> None) then
          distancias.(i).(j) = Some 0
        else
          if (j > 0 && m.(i).(j) <= distancias.(i).(j - 1)) then
            distancias.(i).(j) = m.(i).(j)
          else 
    
    distancias;;




let ejemplo_grafo = [|
    [| None; Some 1; Some 4; Some 7; |];
    [| Some 1; None; Some 2; Some 8 |];
    [| Some 4; Some 2; None; Some 3 |];
    [| Some 7; Some 8; Some 3; None |];
  |];;
  
dijkstra ejemplo_grafo;;
