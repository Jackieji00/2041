let array_sum arr =
    let result = ref 0 in
    for i = 0 to Array.length arr -1 do (*loop over each element, and add then together*)
        result := !result + arr.(i);
    done;
    !result
;;


let rec list_sum lst =
    if lst = [] then            (*base case if list is empty return 0*)
        0
    else
      let head = List.hd lst in
      let rest = List.tl lst in
      if rest = [] then         (*if it is the last element return head*)
          head
      else                      (*add head and recursive rest*)
          head + list_sum rest
;;
