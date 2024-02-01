(* Perfect numbers

   A positive integer `n` is perfect if it is equal to the sum of its
   proper divisors.

   Please implement the function `is_perfect` of type `int -> bool`
   which, given an positive integer `n`, returns `true` if `n` is
   perfect and `false` otherwise.

   Examples:
   let _ = assert (is_perfect 6)        (* 1 + 2 + 3 = 6 *)
   let _ = assert (is_perfect 28)       (* 1 + 2 + 4 + 7 + 14 = 28 *)
   let _ = assert (not (is_perfect 24)) (* 1 + 2 + 3 + 4 + 6 + 8 + 12 <> 24 *)

 *)

let is_perfect (n : int) : bool =
  let rec divisors (div : int) : int =
    if div = 1 then 1 (* if divisor is 1 then return 1 *)
    else if n mod div = 0 && n <> div then div + divisors(div - 1) (* check if the current number is a divisor, if yes add it *) 
    else divisors(div - 1) (* else continue recursion *)
  in if divisors (n) = n then true else false;;



