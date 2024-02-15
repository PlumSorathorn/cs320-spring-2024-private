(* Concatenation Lists

   A `concatlist` is a list whose constructors are based on
   concatenation instead of cons.  It has a constructor for the empty
   list (Nil), a single element list (Single) and the concatentation
   of two lists (Concat).
  
   Implement a function `sort` which given

     l : a `concatlist`

   returns a regular list with the same element as `l` but in sorted
   order.  You should do this WITHOUT trying to first converting `l`
   into a regular list.  In particular, you CANNOT use the function
   `List.sort`.

   Example:
   let l = Concat (Concat (Single 3, Single 2), Concat (Single 1, Single 10))
   let _ = assert (sort l = [1;2;3;10])
*)

type 'a concatlist
  = Nil
  | Single of 'a
  | Concat of 'a concatlist * 'a concatlist

let rec merge left_side right_side = 
  match (left_side, right_side) with
  | [], l -> l
  | l, [] -> l
  | left_n :: left_rest, right_n :: right_rest ->
      if left_n < right_n 
        then left_n :: merge left_rest right_side
      else right_n :: merge left_side right_rest

let sort (l : 'a concatlist) : 'a list =
  let rec sort_and_list (cl : 'a concatlist) : 'a list = 
  match cl with
  | Nil -> []
  | Single x -> [x]
  | Concat (l, r) ->
      let sorted_left = sort_and_list l in
      let sorted_right = sort_and_list r in
      merge sorted_left sorted_right
  in sort_and_list l

let l = Concat (Concat (Single 10, Single 1), Concat (Single 2, Single 3))
let _ = assert (sort l = [1;2;3;10])