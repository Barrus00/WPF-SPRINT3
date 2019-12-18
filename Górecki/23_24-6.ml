let wakacje k r lst =
  let rec loop res ip range = function
    | [] -> res
    | h :: t -> if ip = 0 || range < h then
        loop (res + 1) k (h + 2 * r) (h :: t) else
        loop res (ip - 1) range t
  in
  loop 0 0 0 lst