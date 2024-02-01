(* Reversing strings

Please implement the function `string_rev` of type `string ->
string` which, given a string `s`, returns the string with the same
characters but in reverse order.

Hint: Recall that there are no built-in functions for converting
from `string` to `char or vice versa. See OCP 2.3.1 for details on
how to accomplish this.

Hint: Take a look at the following functions in the OCaml
documentation:

- `String.sub`
- `String.length`

Examples:
let _ = assert (string_rev "testing" = "gnitset")
let _ = assert (string_rev "12345" = "54321")
let _ = assert (string_rev "noon" = "noon")

*)

let string_rev (s : string) : string = 
  let rec reverse (rev_s : string) (i : int)  : string =
    if i < 0 then (*base case, return rev_s*)
      rev_s
    else (*incrementally reverse string by one character*)
      reverse (rev_s ^ String.sub s i 1) (i - 1) 
  in
  reverse "" (String.length s - 1);;
  