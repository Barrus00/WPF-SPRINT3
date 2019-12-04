open Array ;;

let zamien i j arr =
  let pom = arr. (i) in
  begin
    arr. (i) <- arr. (j) ;
    arr. (j) <- pom ;
  end ;;

let zamiany arr = 
  let n = length arr in
  for i = 0 to n-1 do
      while arr. (i) != i do
          zamien arr. (i) i arr;
      done
  done ;;
