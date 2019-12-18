open List;;

let rec sil x n wyn acc=
  if acc * x > n then
    wyn
  else
    sil (x+1) n (((acc*x),x)::wyn) (acc*x) ;;

let silnia n  =
  let rec loop n sol lst = 
    match n with
    | 0 -> sol
    | _ -> 
      let x = fst (hd lst) in
      if x <= n then
        loop (n-x) (snd(hd lst)::sol) lst
      else
        loop n sol (tl lst) in
  let lst = sil  1 n [] 1 in
  loop n [] lst ;; 
