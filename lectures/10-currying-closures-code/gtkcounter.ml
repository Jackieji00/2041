(* gtkcounter.ml: use the GTK library to create a GUI window with a
   button which increases a counter. This file can be built with the
   included Makefile but requires OCaml's lablgtk library to be
   installed and likely only runs on Linux. *)

open Printf;;

let destroy = GMain.Main.quit;;
let _ = 
  let window =                  (* create a window *)
    GWindow.window  ~title:"Button Click Demo"
      ~width:300 ~height:200 ~border_width:10 ()
  in
  let _ = window#connect#destroy ~callback:GMain.Main.quit in
  let button =                  (* create a button in the window *)
    GButton.button ~label:"Not clicked yet" ~packing:window#add ()
  in
  let click_count = ref 0 in    (* ref to count clicks *)
  let click_callback () =       (* what to do when button is clicked *)
    printf "click_callback running\n"; flush_all ();
    click_count := !click_count + 1;
    let msg = sprintf "Cicked %d times" !click_count in
    button#set_label msg
  in
  let _ =                       (* connect clicking to the callback *)
    button#connect#clicked ~callback:click_callback
  in
  window#show ();               (* show the window *)
  GMain.Main.main ()            (* run the GTK main GUI loop *)
;;
