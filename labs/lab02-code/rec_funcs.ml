let rec last_elem list =                      (*a recursive function*)
  if list = [] then                           (*if it is empty list*)
    raise (Failure "No last element in an empty list") (* raise an exception*)
  else                                        (*if it is not empty list*)
    let elem = List.hd list in                (*set first one in list as variable elem *)
    let rest = List.tl list in                (*set the rest part of list as variable rest*)
    if rest = [] then                         (*if it is the last one*)
      elem                                    (*return elem*)
    else                                      (*if it is not the last one*)
      last_elem rest                          (*recursive the rest *)
;;

let elems_outside start stop list =           (*set a function name it elems_outside*)
  let rec helper pos lst =                    (*set a recursive fucntion as helper*)
    if lst=[] then                            (*if the input list is empty*)
      []                                      (*return an empty list *)
    else if start<=pos && pos<=stop then      (*if postion are smaller than start and stop*)
      helper (pos+1) (List.tl lst)            (*keep recursive next*)
    else                                      (*if it not*)
      let elem = List.hd lst in               (*set first one in list as variable elem *)
      let rest = List.tl lst in               (*set the rest part of list as variable rest*)
      let result =  helper (pos+1) rest in    (*set result as next recursive next*)
      elem :: result                          (*attach elem with result*)
  in                                          (*in*)
  helper 0 list                               (*call helper func with inputs 0 and list*)
;;

(* REPL EXAMPLES
   # elems_outside 3 5 [0; 1; 2; 3; 4; 5; 6; 7];;
   - : int list = [0; 1; 2; 6; 7]
   # elems_outside 1 5 [0; 1; 2; 3; 4; 5; 6; 7];;
   - : int list = [0; 6; 7]
   # elems_outside 2 4 [0; 1; 2; 3; 4; 5; 6; 7];;
   - : int list = [0; 1; 5; 6; 7]
   # elems_outside 2 4 [];;
   - : 'a list = []
*)
