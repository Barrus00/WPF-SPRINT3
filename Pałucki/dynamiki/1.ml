open Array;;

let wydaj k nom = 
  let n = length nom in
  let dyn = make (k+1) 0 in
  begin
    for i = 1 to k do
      dyn. (i) <- max_int
    done ;
    dyn. (0) <- 0 ;
    
    for i = 1 to n do 
      let pom = nom. (i-1) in
      for j = 0 to k-pom  do
        if dyn. (j) < max_int then
          if dyn. (j) + 1 < dyn. (j + pom) then
            dyn. (j + pom) <- dyn. (j) + 1 ;
      done
    done
  end ;
  dyn. (k) ;;
