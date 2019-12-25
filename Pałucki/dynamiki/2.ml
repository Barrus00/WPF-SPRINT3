open Array;;

let wydaj k nom = 
  let n = length nom in
  let dyn = make (k+1) 0 in
  begin
    for i = 1 to k do
      dyn. (i) <- 0
    done ;
    dyn. (0) <- 1 ;

    for i = 1 to n do
      let pom = nom. (i-1) in
      for j = pom to k do 
        dyn. (j) <- dyn. (j) + dyn. (j-pom) 
      done
    done
  end ; 
  dyn. (k) ;;
