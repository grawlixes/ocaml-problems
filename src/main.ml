(* All solutions are in the same order as they 
 * appear on the website - top to bottom *)

open Core

(* Some useful functions *)

(* Print various optionals *)
let print_int = function
    | None   -> print_endline "<empty int>"
    | Some x -> print_endline (string_of_int x)
;;
let print_string = function
    | None   -> print_endline "<empty string>"
    | Some s -> print_endline s
;;
let rec print_int_array = function
    | None    -> print_endline "<empty array>"
    | Some [] -> print_endline ""
    | Some (h :: l)  -> 
            Printf.printf "%i, " h;
            print_int_array (Some l)
;;

(* Newline to separate from dune output *)
print_string (Some "");;


(* Problem 1: get tail of list *)
let rec last = function
    | [] -> None
    | [ x ] -> Some x
    | _ :: l -> last l
;;

(* Problem 2: get last two elements of a list *)
let rec last_two = function
    | [] -> None
    | [ _ ] -> None
    | [ x; y ] -> (Some [ x; y ])
    | _ :: l -> last_two l
;;

