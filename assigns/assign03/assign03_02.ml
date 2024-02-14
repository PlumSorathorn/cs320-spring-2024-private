(* Forklist

   A `forklist` is combination of a list and a binary tree.  It has
   constructors for the empty list (Nil), a single link (Cons) and a
   double link (Fork).

   A forklist `l` with DISTINCT elements is ORDERED if it satisfies the
   following properties:

   1. If `l` is of the form `Cons (x, xs)` then every element in `xs` is
   greater than `x`.

   2. If `l` is of the form `Fork (x, lxs rxs)` then every element in
   `lxs` is less than x and every element in `rxs` is greater than
   `x`.

   A forklist `l` is TAILED if it satisfies the property that if `Cons
   (x, xs)` appears in `l`, then `xs` contains no `Fork`s.

   Implement a function `delay_cons` which given

     f : an ordered forklist of integers

   returns a TAILED ordered forklist with the following properties:

   1. It has the same elements as `f`

   2. It has the same number of `Cons`s, `Fork`s and `Nil`s as `f`.

   Example:
   let f = Cons (2, Fork(4, Cons(3, Nil), Cons (5, Nil)))
   let g = Fork (4, Cons (2, Cons (3, Nil)), Cons(5, Nil))
   let _ = assert (delay_cons f = g)

   NOTE: the output does not need to look exactly like this. It just
   has to satisfy the above properties.

*)

type 'a forklist
  = Nil
  | Cons of 'a * 'a forklist
  | Fork of 'a * 'a forklist * 'a forklist

let rec separate_cons_fork (f : 'a forklist) : ('a forklist * 'a forklist) =
  match f with
  | Nil -> (Nil, Nil)
  | Cons (x, xs) ->
    let cons_tail, fork_tail = separate_cons_fork xs in
    (Cons (x, cons_tail), fork_tail)
  | Fork (x, lxs, rxs) ->
    let lxs_cons, lxs_fork = separate_cons_fork lxs in
    let rxs_cons, rxs_fork = separate_cons_fork rxs in
    (Cons (x, lxs_cons), Fork (x, rxs_cons, rxs_fork));;

let delay_cons (f : 'a forklist) : 'a forklist = 
  let rec merge_cons_fork (f : 'a forklist) : 'a forklist =
    let cons_part, fork_part = separate_cons_fork f in
    match cons_part, fork_part with
    | Nil, _ -> Nil
    | _, Nil -> cons_part
    | _ ->
      match fork_part with
      | Nil -> Nil
      | Cons (x, xs) -> Fork (x, cons_part, xs)
      | Fork (x, lxs, rxs) -> Fork (x, cons_part, merge_cons_fork rxs)
    in merge_cons_fork f


let test_case_1 () =
  let f = Cons (2, Fork (4, Cons (3, Nil), Cons (5, Nil))) in
  let g = Fork (4, Cons (2, Cons (3, Nil)), Cons (5, Nil)) in
  assert (delay_cons f = g)

let test_case_2 () =
  let f = Fork (4, Cons (2, Fork(3, Cons(5, Nil), Cons(6, Nil))), Cons(1, Nil)) in
  let g = Fork (4, Fork (3, Cons (2, Nil), Cons (5, Nil)), Cons (1, Nil)) in
  assert (delay_cons f = g)
