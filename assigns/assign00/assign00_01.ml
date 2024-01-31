let sqrt (n : int) : int = 
   (* YOUR CODE GOES HERE *)
   if sqrt(float_of_int(n)) /. 1.0 != 0.00 then truncate(sqrt(float_of_int(n))) + 1
   else truncate(sqrt(float_of_int(n)));;

let _ = print_int(sqrt 99);;