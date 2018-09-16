let array_above thresh arr =
    let count = ref 0 in
    for c = 0 to Array.length arr -1 do   (*measure the result array length*)
        if arr.(c) > thresh then
            count := !count +1
    done;
    let result = Array.make !count thresh in
    let countR = ref 0 in
    for i = 0 to Array.length arr -1 do   (*put qualified elements into the result*)
        if arr.(i) > thresh then
            begin
              result.(!countR) <- arr.(i);
              countR := !countR +1;
            end;
    done;
    result
;;

let rec list_above thresh lst =
  if lst =[] then                    (*base case if lst is empty*)
      lst
  else
      let head = List.hd lst in
      let rest = List.tl lst in
      if rest = [] then              (*if it is the last element*)
          if head > thresh then
            lst
          else                       (*make ocaml expect 'a list' *)
            []
      else                           (* recursive the rest with head if it is qualified elements
                                        else without head *)
          if head > thresh then
            let arest = list_above thresh rest in
            let alist = head :: arest in
            alist
          else
            list_above thresh rest
;;
