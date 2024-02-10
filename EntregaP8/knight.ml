
  
let tour m n obstaculos ini fin =
  let is_valid_move (x, y) =
    x >= 0 && x < m && y >= 0 && y < n && not (List.mem (x, y) obstaculos)
  in

  let rec tour_helper path current_pos =
    if current_pos = fin then
      List.rev (current_pos :: path)
    else
      let (x, y) = current_pos in
      let possible_moves = [
        (x + 1, y + 2);
        (x + 1, y - 2);
        (x - 1, y + 2);
        (x - 1, y - 2);
        (x + 2, y + 1);
        (x + 2, y - 1);
        (x - 2, y + 1);
        (x - 2, y - 1);
      ] in

      let valid_moves = List.filter is_valid_move possible_moves in

      match valid_moves with
      | [] -> raise Not_found
      | moves ->
        let rec try_moves = function
          | [] -> raise Not_found
          | move :: rest ->
            if move = ini || List.mem move path then try_moves rest
            else
              try
                tour_helper (current_pos :: path) move
              with Not_found -> try_moves rest
         in try_moves moves
  in tour_helper [] ini;;
  
  
  
  
let min_tour m n obstaculos ini fin =

  let is_valid_move (x, y) =
    x >= 0 && x < m && y >= 0 && y < n && not (List.mem (x, y) obstaculos)
  in

  let rec bfs queue visited =
    match queue with
    | [] -> raise Not_found
    | (path, current_pos) :: rest ->
      if current_pos = fin then
        List.rev (current_pos :: path)
      else
        let (x, y) = current_pos in
        let possible_moves = [
          (x + 1, y + 2);
          (x + 1, y - 2);
          (x - 1, y + 2);
          (x - 1, y - 2);
          (x + 2, y + 1);
          (x + 2, y - 1);
          (x - 2, y + 1);
          (x - 2, y - 1);
        ] in

        let valid_moves = List.filter is_valid_move possible_moves in

        let new_moves =
          List.filter (fun move -> not (List.mem move visited)) valid_moves in

        let new_paths =
          List.map (fun move -> (current_pos :: path, move)) new_moves in

        bfs (rest @ new_paths) (current_pos :: visited)
  in
  try
    bfs [([], ini)] []
  with Not_found -> raise Not_found;;
  
  
  
let min_tour4D m n obstaculos ini fin =

  let (%) x y = if x >= 0 then x mod y else y + x mod y in
  
  let is_valid_move (x, y) =
    x >= 0 && x < m && y >= 0 && y < n && not (List.mem (x, y) obstaculos)
  in

  let rec bfs queue visited =
    match queue with
    | [] -> raise Not_found
    | (path, current_pos) :: rest ->
      if current_pos = fin then
        List.rev (current_pos :: path)
      else
        let (x, y) = current_pos in
        let possible_moves = [
          ( (x + 1) % m, (y + 2) % n);
          ( (x + 1) % m, (y - 2) % n);
          ( (x - 1) % m, (y + 2) % n);
          ( (x - 1) % m, (y - 2) % n);
          ( (x + 2) % m, (y + 1) % n);
          ( (x + 2) % m, (y - 1) % n);
          ( (x - 2) % m, (y + 1) % n);
          ( (x - 2) % m, (y - 1) % n);
        ] in

        let valid_moves = List.filter is_valid_move possible_moves in

        let new_moves =
          List.filter (fun move -> not (List.mem move visited)) valid_moves in

        let new_paths =
          List.map (fun move -> (current_pos :: path, move)) new_moves in

        bfs (rest @ new_paths) (current_pos :: visited)
  in
  try
    bfs [([], ini)] []
  with Not_found -> raise Not_found;;
  

