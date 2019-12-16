open List

let domino lst =
  let rec optimize (lst1, lst2, best, len) (a, b) =
    let new_sol = (a, b) :: add_block b (lst1 @ tl lst2) in
    let new_len = length new_sol in
    if (new_len > len) then ((a, b) :: lst1, tl lst2, new_sol, new_len) else
      ((a, b) :: lst1, tl lst2, best, len)
  and add_block prev lst =
    if lst = [] then [] else
      let try_new (lst1, lst2, best, len) (a, b) = 
        if prev = a then
          optimize (lst1, lst2, best, len) (a, b)
        else if prev = b then
          optimize (lst1, lst2, best, len) (b, a)
        else ((a, b) :: lst1, tl lst2, best, len)
      in
      let (_, _, res, _) = fold_left try_new ([], lst, [], 0) lst in res
  in
  let (_, _, res, _) = fold_left optimize ([], lst, [], 0) lst in res
