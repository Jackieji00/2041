(* undolist.ml : This module manages a sorted list strings. add,
   remove, and merge operations are provided. undo/redo operations are
   provided to alter list. Type annotaions are required on the
   module-level values as refs to 'a list are not allowed due to weak
   typing.

   All functions in this file pertain to PROBLEM 3
*)

let curr_list : string list ref = ref []
;;
(* The current list of strings. *)

let undo_stack : string list list ref = ref []
;;
(* List of previous curr_lists to enable undo. *)

let redo_stack : string list list ref = ref []
;;
(* List of undone curr_lists to enable redo. *)

let reset_all () =
  curr_list := [];
  undo_stack := [];
  redo_stack := []
;;
(* Reset curr_list, undo_stack, redo_stack to all be empty lists. Used
   only in testing, not in main programs. *)


let set_to_list new_list =
  redo_stack :=[];                         (*Empties redo_stack*)
  undo_stack := !curr_list :: !undo_stack; (*curr_list is moved to the top of the undo_Stack*)
  curr_list := new_list                    (*curr_list is set to the new_list. *)
;;
(* curr_list is moved to the top of the undo_Stack. Then curr_list is
   set to the new_list. Empties redo_stack. *)

let add_elem elem =
  let new_list = (Sortedlist.insert !curr_list elem) in
  set_to_list new_list
;;
(* Add the given elem to curr_list producing a new_list.  Calls
   set_to_list on result to unable undoing. *)

let remove_elem elem =
  let new_list = (Sortedlist.remove !curr_list elem) in
  set_to_list new_list
;;
(* Remove the given elem from curr_list producing a new list. Calls
   set_to_list on result to unable undoing.  *)

let merge_with_list list =
  let new_list = (Sortedlist.merge !curr_list list) in
  set_to_list new_list
;;
(* Merge the param list with the current list. Calls set_to_list on
   the result to enable undoing. *)

let undo () =
  if undo_stack != ref [] then                     (*if the undo_stack is not empty*)
    begin                                          (*the top element of undo_stack is removed and becomes curr_list*)
      if (!redo_stack = []&& !curr_list =[]) then  (* if both curr_list and redo_stack is empty*)
        redo_stack := []                           (* set redo_stack empty list *)
      else if !curr_list != [] then
        redo_stack := !curr_list::!redo_stack;     (*else current become redo_stack*)
      match !undo_stack with
      | head::tail ->
        undo_stack := tail;                        (*head of undo_stack is removed and become curr_list*)
        curr_list := head;
        true                                       (*return true*)
      | [] -> false
    end
  else
    false                                          (*rest all return false*)
;;
(* If the undo_stack is not empty, undo the last operation. curr_list
   is moved to the redo_stack and the top element of undo_stack is
   removed and becomes curr_list.  Returns true if changes are made
   and false if undo_stack is empty and no changes are made. Operates
   in constant time. *)

let redo () =
  if !redo_stack != [] then                   (*If the redo_stack is not empty*)
    begin                                     (*redo the last operation*)
    undo_stack := !curr_list::!undo_stack;    (*curr_list is moved to the undo_stack*)
    let tail = List.tl !redo_stack in
    let head = List.hd !redo_stack in
    redo_stack := tail;                       (*the top element of redo_stack is removed and becomes curr_list*)
    curr_list := head;
      true                                    (*return true*)
    end
  else
    false                                     (*rest situation return false*)
;;
(* If the redo_stack is not empty, redo the last operation. curr_list
   is moved to the undo_stack and the top element of redo_stack is
   removed and becomes curr_list.  Returns true if changes are made
   and false if redo_stack is empty and no changes are made. Operates
   in constant time. *)
