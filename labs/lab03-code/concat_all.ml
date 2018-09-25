let rec concat_all_crap strlist =
  if strlist=[] then
    ""
  else
    let head = List.hd strlist in
    let tail = List.tl strlist in
    let rest = concat_all_crap tail in
    head ^ " " ^ rest
;;

let rec concat_all_good strlist =
  if strlist=[] then
    ""
  else
    let head = List.hd strlist in
    let tail = List.tl strlist in
    if tail=[] then
      head
    else
      let rest = concat_all_good tail in
      head ^ " " ^ rest
;;

let concat_all strlist =
  let rec helper strlist =
    match strlist with
    | [] -> ""
    | head ::[]-> head
    | head :: tail-> head ^ " " ^ helper tail
  in helper strlist
;;
