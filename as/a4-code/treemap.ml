(* treemap.ml: Provides a functor to create a map from an arbitrary
   key type to an arbitrary value type. Functor Make requires a module
   input matching the KEYVAL_SIG signature.  The resulting module
   has the same functions as Ssmap but that works for maps with the
   specified key/value types.  As the name implies, a BST is the
   underlying data structure for the map.
*)

(* Type of key/vals that go into Treemaps. Includes key and value
   type, key comparison function, and a string function for easy
   display. *)
module type KEYVAL_SIG =
sig
  type key_t
  type value_t
  val compare_keys : key_t -> key_t -> int
  val keyval_string : key_t -> value_t -> string
end;;

(* Functor which creates a module for maps with key/value types
   specified by the parameter module KVMod. *)

module Make (KVMod : KEYVAL_SIG) =
struct
  open Printf;;

  type treemap =
    | Empty               (* no data: bottom of tree   *)
    | Node of {           (* node of anonymous record  *)
        key   : KVMod.key_t;   (* key for this node         *)
        value : KVMod.value_t;   (* value associated with key *)
        left  : treemap;    (* left branch               *)
        right : treemap;    (* right branch              *)
      }
  ;;

  let empty = Empty;;

  let rec add map key value =
    match map with
    | Empty ->                                             (* bottom of tree: didn't find *)
       Node{key=key; value=value;                          (* make a new node with key/val binding *)
            left=Empty; right=Empty}
    | Node(node) ->                                        (* at a node *)
       let diff = KVMod.compare_keys key node.key in           (* compute a difference *)
       if diff = 0 then                                    (* 0 indicates equal *)
         Node{node with value=value}                       (* replace value binding with new value *)
       else if diff < 0 then                               (* negative indicates str less than data *)
         Node{node with left=(add node.left key value)}    (* create a new node with new left branch *)
       else                                                (* positive indicates str greater than data *)
         Node{node with right=(add node.right key value)}  (* create new node with new right branch *)
  ;;

  let tree_string map =
    let buf = Buffer.create 256 in                    (* extensibel character buffer *)

    let rec build tree depth =                        (* recursive helper *)
      match tree with
      | Empty -> ()                                   (* out of tree, done with this branch *)
      | Node(node) ->                                 (* have a node *)
         build node.right (depth+1);                  (* recurse on right branch *)
         for i=1 to depth do                          (* indent according to depth of this node *)
           Buffer.add_string buf "  ";
         done;
         let kv = KVMod.keyval_string node.key node.value in
         let datastr =                                (* string with depth and data  *)
           sprintf "%2d: %s\n" depth kv
         in
         Buffer.add_string buf datastr;               (* add to buffer *)
         build node.left (depth+1);                   (* recurse on left branch *)
    in                                                (* end helper *)

    build map 0;                                      (* recurse from root *)
    Buffer.contents buf                               (* return string from Buffer *)
  ;;

  let rec getopt map key =
    match map with
    | Empty -> None
    | Node(node) ->
       let diff = KVMod.compare_keys key node.key in
       if diff = 0 then
         Some node.value
       else if diff < 0 then
         getopt node.left key
       else
         getopt node.right key
  ;;

  let contains_key map str =
    let result = getopt map str in
    if result = None then
      false
    else
      true
  ;;

  let rec iter func map =
    match map with
    | Empty -> ()
    | Node(node) ->
        iter func node.left;
        func node.key node.value;
        iter func node.right
  ;;

  let rec fold func cur map =
    match map with
    | Empty -> cur
    | Node(node) ->
        let next = fold func cur node.left in
        let n = func next node.key node.value in
        fold func n node.right
  ;;

  let to_string map =                                 (* verbose version: no use of iter *)
    let buf = Buffer.create 256 in
    Buffer.add_string buf "[";
    let funcStr k v =
      let skv = KVMod.keyval_string k v in
      let str = sprintf "%s, " skv in
      Buffer.add_string buf str
    in
    iter funcStr map;
    if not (map = Empty) then
      Buffer.truncate buf ((Buffer.length buf)-2);
    Buffer.add_string buf "]";
    Buffer.contents buf
  ;;

  let rec findmin_keyval map =
    match map with
    | Empty -> failwith "No minimum in an empty tree"
    | Node(node) when node.left = Empty->
      (node.key,node.value)
    | Node(node) -> findmin_keyval node.left
  ;;

  let rec remove_key map key =
    match map with
    | Empty -> map
    | Node(node) ->
      let diff = KVMod.compare_keys key node.key in
      if diff = 0 then
        if (node.left = Empty)&&(node.right = Empty) then
          Empty
        else if (node.left = Empty) then
          node.right
        else if (node.right = Empty) then
          node.left
        else
          let rec help map =
            match map with
            | Empty -> Empty
            | Node(node1)->
              if node1.left=Empty then
                begin
                  let rest = remove_key node.right node1.key in
                  Node{key=node1.key;value=node1.value;left=node.left;right=rest}
                end
              else
                help node1.left
          in help node.right
      else if diff < 0 then
        Node{node with left=(remove_key node.left key)}
      else
        Node{node with right=(remove_key node.right key)}
    ;;
end;;
