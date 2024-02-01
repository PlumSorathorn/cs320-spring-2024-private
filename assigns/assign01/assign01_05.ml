(* Block text

   Please implement the function `block_text` of type `string ->
   string` which, given

   - a string `s` consisting only of capital English characters A-Z
   - a nonnegative integer `min_width`
   - a positive integer `max_width`

   returns a string with the same characters as `s` but separated into
   lines with the following properties:

   - each line has length at most `max_width`
   - each line has the same length except possibly the last line
   - the last line has length at least `min_width`

   If the above three properties are not possible to satisfy, then
   every line except for the last line of the returned string should
   have length `max_width` (in other words, ignore the last
   condition).

   If there are multiple ways to satisfy the above three properties,
   the choice with the longest lines (besides the last line) should be
   used.

   Hint: Take a look at the following functions in the OCaml
   documentation:

   - `String.sub`
   - `String.length`

   Examples:
   let _ = assert (block_text "ABCDEFGHIJ" 0 3 = "ABC\nDEF\nGHI\nJ")
   let _ = assert (block_text "ABCDEFGHIJ" 2 3 = "AB\nCD\nEF\nGH\nIJ")
   let _ = assert (block_text "ABCDEFGHIJ" 0 4 = "ABCD\nEFGH\nIJ")
   let _ = assert (block_text "ABCDEFGHIJ" 3 4 = "ABCD\nEFGH\nIJ")

 *)

let rec split current_width s =
  if String.length s <= current_width then s
  else String.sub s 0 current_width ^ "\n" ^ split current_width (String.sub s current_width (String.length s - current_width))

let block_text (s : string) (min_width : int) (max_width : int) : string =
  let rec width_recursion (len : int) (min : int) (max : int) (i : int ) : int = 
    if (max - i) < min then max
    else if (len - 1) mod (max - i) + 1 >= min_width then (max - i)
    else width_recursion len min max (i + 1)
  in split (width_recursion (String.length s) min_width max_width 0) (s)

  
  let () = print_string (block_text "ABCDEFGHIJKLM" 3 12); print_newline (); print_newline ();;
  print_string (block_text "ABCDEFGHIJ" 0 3); print_newline (); print_newline ();;
  print_string (block_text "ABCDEFGHIJ" 2 3); print_newline (); print_newline ();;
  print_string (block_text "ABCDEFGHIJ" 0 4); print_newline (); print_newline ();;
  print_string (block_text "ABCDEFGHIJ" 3 4); print_newline (); print_newline ();;
