(* All solutions are in the same order as they 
 * appear on the website - top to bottom *)

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
let print_bool = function
    | false -> print_endline "false"
    | true  -> print_endline "true"
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

(* Problem 3: get the n-th element of a list *)
let rec nth i = function
    | [] -> None
    | h :: t -> if i = 0 then Some h else nth (i - 1) t;;

(* Problem 4: get the length of a list (TR) *)
let length l =
    let rec aux n = function
      | [] -> n
      | _ :: t -> aux (n + 1) t
    in
    aux 0 l
;;

(* Problem 5: reverse a list (TR) *)
let rev l =
    let rec aux acc = function
      | [] -> acc
      | h :: t -> aux (h :: acc) t
    in
    aux [] l
;; 

(* Problem 6: check if a list is a palindrome *)
let is_palindrome l =
    l = rev l
;;

print_bool (is_palindrome [1;2;3])
