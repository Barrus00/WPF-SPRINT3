open Array ;; 

let autobus mat  =
  let n = length mat in
  let m = length mat. (0) in
  begin 
    for i = 1 to n-1 do
      mat. (i). (0) <- mat. (i-1). (0) + mat. (i). (0);
    done ;
    for j = 1 to m-1 do
      mat. (0). (j) <- mat.(0). (j-1) + mat. (0). (j);
    done ;
    for i= 1 to n-1 do
      for j = 1 to m-1 do 
        mat. (i). (j) <-( max (mat. (i-1). (j)) (mat. (i). (j-1))) + mat. (i). (j) ;
      done
    done
  end ;
  mat. (n-1). (m-1) ;;
