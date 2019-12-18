let rec generate n a b lst =
  if a + b > n then lst else
    generate n (a + b) a ((a + b) :: lst)


let ciag x =
  let fib = generate x 1 0 [1] in
  let rec loop x (res_h :: res_t) = function
    | [] -> failwith "Empty Fibonacci list"
    | h :: t -> if x = 0 then (res_h :: res_t) else
      if h > x then
        loop x (res_h :: res_t) t else
        loop (x - h) ((res_h + h) :: res_h :: res_t) t
  in
  List.rev (loop (x - 1) [1] fib)