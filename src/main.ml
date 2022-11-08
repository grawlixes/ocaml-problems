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
    | Some (h :: t)  -> 
            Printf.printf "%i, " h;
            print_int_array (Some t)
;;
let rec print_2d_int_array = function
    | None    -> print_endline "<empty array>"
    | Some [] -> print_endline "";
    | Some (h :: t)  ->
            print_int_array (Some h); 
            print_2d_int_array (Some t)
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

(* Problem 6: check if a list is a palindrome 
 * using core screws this up and causes a type
 * mismatch, I think because it messes with the
 * equals operator. I'll stop using it for now *)
let is_palindrome l =
    l = (rev l)
;;

(* Problem 7: flatten a nested list structure *)
type 'a node =
  | One of 'a 
  | Many of 'a node list
;;

let flatten l = 
    let rec aux acc = function
        | []            -> acc
        | One x :: t    -> aux (x :: acc) t
        | Many lst :: t -> aux (aux acc lst) t
    in
    rev (aux [] l)
;; 

(* Problem 8: remove CONSECUTIVE duplicates from a list 
 * my implementation isn't as pretty but it's tail recursive*)
let compress list =
    let rec aux prev acc = function
        | []     -> acc
        | x :: t -> if (x = prev) then (aux prev acc t)
                    else (aux x (x :: acc) t)
    in
    rev (aux 0 [] list)
;;

(* beautiful implementation of problem 8 from the wiki
let rec compress = function
    | a :: (b :: _ as t) -> if a = b then compress t 
                            else a :: compress t
    | single_element -> single_element
;;
*)

(* Problem 9: pack consecutive duplicates into sublists 
 * the wiki actually does it similarly, not as pretty as above *)
let pack list =
     let rec aux prev cur acc = function
         | []     -> cur :: acc
         | x :: t -> if (x = prev) then (aux prev (x :: cur) acc t)
                     else (aux x [x] (if prev != 0 then cur :: acc 
                                      else acc) 
                           t)
     in
     rev (aux 0 [] [] list)
;;


(* Problem 10: run-length encoding *)
let rle list =
    let rec aux prev count acc = function
        | []     -> if count != 0 then [count; prev] :: acc
                    else acc
        | h :: t -> if (h = prev) then (aux prev (count + 1) acc t) 
                    else (aux h 1 (if count != 0 then ([count; prev] :: acc)
                                   else acc) 
                          t)
    in
    rev (aux 0 0 [] list)
;;

print_2d_int_array (Some (rle [1;2;2;3;3;3;4;4;4;4;5;5;5;5;5;1]));;
