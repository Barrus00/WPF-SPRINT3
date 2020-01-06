open Array;;
open Queue;;

let bfs tab n m (y,x) num vis = 
  if tab. (x). (y) < num then
    false
  else 
    let q = Queue.create () in
    push (x,y) q ;
    vis. (x). (y) <- true ; 
    let ans = ref false in
    while !ans = false && not (is_empty q) do 
      let (x,y) = top q in
      pop q ;
      if x = 0 || x = n-1 || y = 0 || y = m-1 then
        ans := true 
      else
        begin
          if vis. (x-1). (y) = false && tab. (x-1). (y) >= num then
            push (x-1,y) q;
            vis. (x-1). (y) <- true;
          if vis. (x+1). (y) = false && tab. (x+1). (y) >= num then
            push  (x+1,y) q ;
            vis. (x+1). (y) <- true;
          if vis. (x). (y-1) = false && tab. (x). (y-1) >= num then
            push  (x,y-1) q ;
            vis. (x). (y-1) <- true;
          if vis. (x). (y+1) = false && tab. (x). (y+1) >= num then
            push  (x,y+1) q ;
            vis. (x). (y+1) <- true;
          end
    done ;
    !ans ;;

let check tab (x,y) num =
  let n = Array.length tab in
  let m = Array.length (tab. (0)) in 
  let vis = make_matrix n m false in
  bfs tab n m (x,y) num vis ;;

let sadzawka tab (x,y)=
  let n = Array.length tab in
  let m = Array.length (tab. (0)) in
  let r = ref (m * n) in
  let l = ref 1 in 
  let sr = ref 0 in
  let f = check tab (x,y) in
  while !l < !r do
    begin
      sr := (!l + !r + 1)/2 ;
      if f !sr then
        l := !sr 
      else
        r := !sr - 1 
    end
  done ;
  !r ;;
  
