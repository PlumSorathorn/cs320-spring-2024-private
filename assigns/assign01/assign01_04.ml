(* Taxicab numbers

   A taxicab number is a positive integer which can be expressed as
   the sum of two positive cubes in more than one way. (This is
   slightly different than the usual definition.)

   Please implement the function `taxicab` of type `int -> int` which,
   given a positive integer `n`, returns the the number of ways that
   `n` can be expressed as the sum of two positive cubes.

   Examples:
   let _ = assert (taxicab 2 = 1)
   let _ = assert (taxicab 5 = 0)
   let _ = assert (taxicab 1729 = 2)   (* 1729 = 1^3 + 12^3 = 3^3 + 9^3 *)
   let _ = assert (taxicab 4104 = 2)   (* 4104 = 2^3 + 16^3 = 9^3 + 15^3 *)

 *)


let rec inner i j n = (* inner loop to both loop the 2nd number and check for the taxicab number *)
  if j > i then 0
  else if (i * i * i) + (j * j * j) = n then
    1 + inner i (j + 1) n
  else inner i (j + 1) n

let taxicab (n : int) : int = (* outer loop that calls the inner loop *)
  let rec outer i n =
    if i * i * i > n then 0
    else inner i 1 n + outer (i + 1) n
  in outer 1 n

