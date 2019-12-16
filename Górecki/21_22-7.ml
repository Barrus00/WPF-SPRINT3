let klej lst =
  let n = List.length lst in
  let pref = Array.make (n + 1) 0 in
  Array.iteri (fun i x -> pref.(i + 1) <- pref.(i) + x) (Array.of_list lst);
  let arr = Array.make_matrix n n 0 in
  for l = 1 to n - 1 do
    for i = 0 to n - 1 - l do
      let j = i + l in begin
        arr.(i).(j) <- max_int;
        for k = i to j - 1 do
          arr.(i).(j) <- min arr.(i).(j)
              (arr.(i).(k) + arr.(k + 1).(j) +
               max (pref.(k + 1) - pref.(i)) (pref.(j + 1) - pref.(k + 1)))
        done
      end
    done
  done;
  arr.(0).(n - 1)