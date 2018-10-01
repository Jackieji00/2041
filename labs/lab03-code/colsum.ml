let rec colsum_nt n =
  Printf.printf "%d\n" n;
  if n = 1 then
    1
  else
    let next =
      if n mod 2 = 0
      then n/2
      else 3*n+1
    in
    let rest = colsum_nt next in
    n + rest
;;

let sum = colsum_nt 10 in
Printf.printf "sum: %d\n" sum;
;;

let colsum_tr n =
  let rec helper result input =
    if input = 1 then
      result+1
    else
      let input0 =
        if input mod 2 = 0 then
          input/2
        else 3*input+1
      in
      let r = result+input in
      helper r input0
  in helper 0 n
;;
let sumtr = colsum_tr 10 in
Printf.printf "sum: %d\n" sumtr;
;;
