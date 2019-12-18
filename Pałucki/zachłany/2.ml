open List ;;

let comp (x,y) (z,w) =
  if y<w then
    -1
  else if y = w then
    0
  else
    1 ;;
    

let deska lst = 
  let rec loop lst last sol = 
    match lst with
    | [] -> sol
    | h::t ->
      if fst h <= last then
        loop t last sol
      else
        loop t (snd h) (sol+1)  in
  let lst = sort comp lst in
  loop lst (-100000000) 0 ;;
  


let l = [(9,11);(7,10);(6,8);(3,6);(2,5);(1,4)];;
