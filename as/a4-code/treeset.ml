(* treeset.ml: provides a Make functor to create a set of unique
   elments given a parameter module adhering to the ELEM_SIG
   signature.  Internally, uses a Treemap to facilitate set
   operations. *)

(* Type for elements that can go into Treesets. Comprised of an
   element type, a comparison function, and *)
module type ELEM_SIG = sig
  type element;;
  val compare : element -> element -> int;;
  val elem_string : element -> string;;
end;;

(* Functor to create a set module *)
module Make (ElMod : ELEM_SIG) = struct

  (* Internal module used to inteface with Treeset.Make *)
  module ElemKeyVal = struct
    type key_t = ElMod.element;;
    type value_t = unit;;
    let compare_keys = ElMod.compare;;
    let keyval_string k v = ElMod.elem_string k;;
  end;;

  (* Internal module providing map functions *)
  module ElTreemap = Treemap.Make(ElemKeyVal)

  (* Empty set value *)
  let empty = ElTreemap.empty;;

  (* Return a set with the given element added. *)
  let add set elem =
    ElTreemap.add set elem ()
  ;;

  (* Produce a string version of the set showing its tree structure. *)
  let tree_string set =
    ElTreemap.tree_string set
  ;;

  (* Return (Some elem) if elem is in the set and None otherwise. *)
  let getopt set elem =
    let op = ElTreemap.getopt set elem in
    match op with
    | None -> None
    | Some a -> Some elem
  ;;

  (* Return true if elem is in the set and false otherwise. *)
  let contains set elem =
    ElTreemap.contains_key set elem
  ;;

  (* Higher order iterate function on a set for side-effects. func
     accepts one argument, an element from the set. *)
  let iter func set =
    let kvfunc k v = func k in
    ElTreemap.iter kvfunc set
  ;;

  (* Higher order folding function on a set. func accepts two
     argument, the current result and an element from the set. *)
  let fold func init set =
    let kvfunc c k v = func c k in
    ElTreemap.fold kvfunc init set
  ;;

  (* Create a string version of the set. *)
  let to_string set =
    ElTreemap.to_string set
  ;;

  (* Return a set with the given element removed. *)
  let remove set elem =
    ElTreemap.remove_key set elem
  ;;
end;;
