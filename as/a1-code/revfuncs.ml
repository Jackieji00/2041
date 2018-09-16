let array_rev arr =
  let len = Array.length arr in
  for i = 0 to len/2 -1 do
    let x = arr.(i) in          (*swap last one and first*)
    let y = arr.(len-1-i) in
    arr.(i) <- y;
    arr.(len-1-i) <- x;
  done;
;;


let list_rev lst =
  let rec helper result alst =
    if alst = [] then                   (*if unsorted list is empty,return the result*)
      result
    else
      let head = List.hd alst in
      let rest = List.tl alst in
      if rest = [] then                 (*if unsorted list only left one, attach the one infront of result*)
        head :: result
      else                              (*after attaching the head infront of result, recursive the rest*)
        helper (head::result) rest
    in
    helper [] lst;
;;
