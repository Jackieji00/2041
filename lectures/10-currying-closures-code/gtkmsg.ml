(* lablgtkmessage.ml: use the GTK library to create a GUI window with
   a message specified. This file can be built with the included
   Makefile but requires OCaml's lablgtk library to be installed and
   likely only runs on Linux. *)

(* User Params *)
let text = ref "Default Message";;
let title = ref "Lablgtk Message";;
let timeout = ref 0;;
let center = ref false;;
let wrap = ref false;;
let fontsize = ref 14;;
let fontweight = ref "normal";;
let width = ref 300;;
let height = ref 100;;
let buttontext = ref "OK";;
let fg = ref "";;
let bg = ref "";;
let randcolors = ref false;;
let bold = ref false;;

(* Constants *)
let buttonheight = 60;;
let bordwidth = 5;;
let maxrgb = 256;;
open Printf;;
open Arg;;
let usg = Printf.sprintf "usage: %s [options] [Message Text]" (Filename.basename Sys.executable_name);;
let default_fn s = text := s;;
let command_options' = [
  "-title",Set_string(title),"string Title of the window";
  "-timeout",Set_int(timeout),"int Timeout in seconds";
  "-center",Set(center)," Center the window";
  "-wrap",Set(wrap)," Wrap text in the window";
  "-fontsize",Set_int(fontsize),"int Font size of Message";
  "-fontweight",Set_string(fontweight),"string Font weight of Message (normal,bold)";
  "-width",Set_int(width),"int Width of window";
  "-height",Set_int(height),"int Height of window";
  "-button",Set_string(buttontext),"string Button text";
  "-randcolors",Set(randcolors)," Select random colors for the message fg/bg";
  "-fg",Set_string(fg),"color Set text color";
  "-bg",Set_string(bg),"color Set text background color";
];;

let command_options = align command_options';;
Random.self_init();;

let destroy = GMain.Main.quit;;
let _ = begin
  parse command_options default_fn usg;
  (* if !text = "" then 
   *   begin Printf.printf "%s\n" usg; exit 0; end; *)
  if !randcolors then begin
    let r,g,b =(Random.int maxrgb, Random.int maxrgb, Random.int maxrgb) in
    fg := sprintf "#%02x%02x%02x" r g b;
    bg := sprintf "#%02x%02x%02x" (maxrgb-r) (maxrgb-g) (maxrgb-b);
  end;
  let fgcolor = 
    if !fg = "" then !fg
    else sprintf "foreground=\"%s\"" !fg in
  let bgcolor =
    if !bg = "" then !bg 
    else sprintf "background=\"%s\"" !bg in
  let pos = if !center then `CENTER else `NONE in
  let window = GWindow.window ~width:!width ~height:!height ~title:!title
      ~border_width:bordwidth ~position:pos () in
  let _ = window#connect#destroy ~callback:GMain.Main.quit in
  let box1 = GPack.vbox ~width:!width ~height:!height ~packing:window#add () in
  let mytext = sprintf "<span font_desc=\"%d\" weight=\"%s\" %s %s >%s</span>"
      !fontsize !fontweight fgcolor bgcolor !text in
  let _ = GMisc.label ~height:(!height-buttonheight-2*bordwidth) ~width:(!width-2*bordwidth)
      ~markup:mytext ~packing:box1#add ~line_wrap:!wrap () in
  let button = GButton.button (* ~width:60 ~height:30 *) ~label:!buttontext ~packing:box1#add () in
  let _ = button#connect#clicked ~callback:GMain.Main.quit in
  window#show ();
  if !timeout > 0 then
	ignore(GMain.Timeout.add ~ms:(!timeout*1000) ~callback:(fun ()->GMain.Main.quit (); true))
  else ();
  GMain.Main.main ()
end
