let crono f x = 
  let t = Sys.time() in
  let _ = f x in
  Sys.time () -. t;;
  
