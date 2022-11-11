(* Problem: generate a list of all tuples of integers in
 * range (0, N-1) without loops. So for N = 3, generate 
 * [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0),
 *  (2, 1), 2, 2)]. 
 * 
 * User NULL#9971 from the OCaml discord helped me with this,
 * a lot of this code is theirs *)

(* Simple tail-recursive implementation that I came up with *)
let generate_pairs_tail_recursive n =
    let rec aux acc x = function
        | 0 ->
            if (x = n) then acc
            else (aux acc (x-1) n)
        | y ->
            aux ((x, y) :: acc) x (y - 1)
    in
     aux [] n 0
;;

(* First implementation given by NULL, this shit is crazy *)
let (let*) a f = List.flatten (List.map f a)
let return a = [a]
let generate_pairs n =
  let* a = List.init n Fun.id in
  let* b = List.init n Fun.id in
  return (a, b)

(* let* is syntactic sugar. I translated it to the following
 * using this wiki: 
     * https://v2.ocaml.org/manual/bindingops.html
 * You could rewrite the below function with some other name
 * for let* and it would still work, but you couldn't do that
 * for the definition above, because the sugar is needed for
 * "in" to work in the above case 
 * also, return (a, b) is a great example of writing 
 * self-documenting code, but I'm being more explicit here *)
let map_and_flatten f a = List.flatten (List.map f a);;
let generate_pairs_for_dummies n =
  map_and_flatten (List.init n Fun.id)
    (fun a ->
      map_and_flatten (List.init n Fun.id)
        (fun b -> [(a, b)])
;l

(* Try to think about what happens if you remove the
 * List.flatten from either application of map_and_flatten,
 * or from both. I think this is a good way to help you
 * understand what's going on. *)

(* List.init n Fun.id just generates a list [0, N-1].
 * so what's happening is this:
     * for every element a in [0,N-1], we are generating a 
     * flat list of [(a, 0), (a, 1), ..., (a, n-1)].
     * Then we combine all these into another list and flatten that too.
 * But the way we implement it is kind of insane, because we
 * get two dimensions of data by defining only two functions 
 * for generating the actual tuples (obviously not including
 * the map/flatten functions). 
 *
 * NULL says two insightful things about this:
     * You need to understand monads to fully grasp this
     * You need to make sure the operation follows the monadic
     *  laws (left/right identity and associativity) or else
     *  its application won't make sense. OCaml doesn't care
     *  if your monad works, just that it follows its rules
 * *) 
