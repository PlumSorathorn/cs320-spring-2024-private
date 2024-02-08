(* Icy Hot

   Implement the function `reduce` which given

     l : a list of `temp`s

   returns a new list of `temp`s gotten by the following reduction rule:

   If `Hot i` and `Icy i` are adjacent (in particular, they must be
   carrying the same value) in any order, then they cancel out and are
   removed from the list.

   This rule should be carried out until it not possible to reduce the
   list any further.

   Examples:
   let _ = assert (reduce [Hot 0;Icy 0] = [])
   let _ = assert (reduce [Hot 0;Icy 1;Hot 1;Icy 0] = [])
   let _ = assert (reduce [Hot 0;Hot 1;Icy 2;Hot 2;Hot 0;Icy 1] = [Hot 0;Hot 1;Hot 0;Icy 1])

*)

type temp
  = Hot of int
  | Icy of int

let rec find_pair l (last_val : temp) (before : temp list) (before_2 : temp list) : temp list =
  match l with
  | [] -> [Hot (-99)]
  | Hot n :: rest -> 
    if last_val = Icy n then (before_2 @ rest) 
    else find_pair rest (Hot n) (before @ [Hot n]) (before)
  | Icy n :: rest -> 
    if last_val = Hot n then (before_2 @ rest) 
    else find_pair rest (Icy n) (before @ [Icy n]) (before)

let reduce (l : temp list) : temp list = 
  let rec loop_pairs (new_list : temp list) : temp list =
    let deleted_pair = (find_pair new_list (Hot (-99)) [] []) in
    if deleted_pair = [] then []
    else if deleted_pair = [Hot (-99)] then new_list
    else loop_pairs(deleted_pair)
  in loop_pairs (l);;


