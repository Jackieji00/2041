(* lexer.ml: basic lexer for a small arithmetic language of ints, +,
   *, (). Add on handling of -, /, variable identifiers. *)

open Printf;;

(* algebraic types for tokens. Add on token types for subtraction, division, and variable identifiers. *)
type token =
  | Plus | Times | OParen | CParen | Int of int
(* ADD IN constructors for subtraction, division, variable id tokens *)
;;

(* rote examples of input with expected output *)
let input = "5 + 10 * height + 7*(3+weight)";; (* Lexing: convert this string..  *)
let lexed = [Int 5; Plus; Int 10;              (* Into this stream of tokens *)
             Times; Ident "height"; Plus;
             Int 7; Times;
             OParen; Int 3; Plus;
             Ident "weight"; CParen];;

(* true if the given character is a digit 0-9 and false otherwise *)
let is_digit c =
  let digits = "0123456789" in
  let loc = String.index_opt digits c in
  loc <> None
;;

(* true if character is letter a-z or A-Z, false otherwise *)
let is_letter c =
  let letters = "abcdefghijklmnopqrstuvwxyz" in
  let lower = Char.lowercase_ascii c in
  let loc = String.index_opt letters lower in
  loc <> None
;;

let lex_string string =                      (* create a list of tokens  *)
  let len = String.length string in
  let rec lex pos =                          (* recursive helper *)
    if pos >= len then
      []
    else
      match string.[pos] with                (* match a single character *)
      |' ' | '\t' | '\n' -> lex (pos+1)
      |'-' -> Minus :: (lex (pos+1))     (* skip whitespace *)
      |'+' -> Plus :: (lex (pos+1))          (* ADD IN recognition of subtraction and division  *)
      |'*' -> Times :: (lex (pos+1))
      |'/' -> Slash :: (lex (pos+1))
      |'(' -> OParen :: (lex (pos+1))        (* and open/close parens *)
      |')' -> CParen :: (lex (pos+1))
      | d when is_digit d ->                 (* see a digit *)
         let stop = ref pos in               (* scan through until a non-digit is found *)
         while !stop < len && is_digit string.[!stop] do
           incr stop;
         done;
         let numstr = String.sub string pos (!stop - pos) in (* substring is the int *)
         let num = int_of_string numstr in   (* parse the integer *)
         Int(num) :: (lex !stop)             (* and tack onto the stream of tokens *)

      (* ADD IN a case for variable identifiers; similar to digit
         recognition above; make use of the is_letter function *)
      | s when is_letter s ->
          let stop = ref pos in               (* scan through until a non-digit is found *)
          while !stop < len && is_letter string.[!stop] do
            incr stop;
          done;
          let char0 = String.sub string pos (!stop - pos) in (* substring is the int *)
          Ident(char0) :: (lex !stop) 
      | _ ->                                 (* any other characters lead to failures *)
         let msg = sprintf "lex error at char %d, char '%c'" pos string.[pos] in
         failwith msg
  in                                         (* end helper *)
  lex 0                                      (* call helper *)
;;

(* AFTER MODIFICATION rote examples of input with expected output *)
(*
let input = "5 + 10 * height + 7*(3+weight)";; (* Lexing: convert this string..  *)
let lexed = [Int 5; Plus; Int 10;              (* Into this stream of tokens *)
             Times; Ident "height"; Plus;
             Int 7; Times;
             OParen; Int 3; Plus;
             Ident "weight"; CParen];;
*)

(* Non-interactive tests for lex_string which should work after it has
   been extended to accept  -, /, and string identifiers*)

(* UNCOMMENT to check that modifications to the lexer are working  *)

(*
let actual1 = lex_string "+ * ( ) 123 -  /  abc";;
let expect1 = [Plus; Times; OParen; CParen; Int(123);
               Minus; Slash; Ident("abc")];;
let ok1 = actual1 = expect1;;

let actual2 = lex_string "(a + b + c) / (9 * d - 10 / 4)";;
let expect2 = [OParen; Ident "a"; Plus; Ident "b"; Plus; Ident "c"; CParen; Slash;
               OParen; Int 9; Times; Ident "d"; Minus; Int 10; Slash; Int 4; CParen]
let ok2 = actual1 = expect1;;
*)
