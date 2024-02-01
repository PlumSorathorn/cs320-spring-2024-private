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

let rec countdown (len : int)(max : int)(min : int)(k : int) : int =
  if k > len then 0
  else if len - (max - k) = min then k
  else countdown len max min (k + 1);;

let rec split (s : string) (min_width : int) (max_width : int) (i : int) : string =
  let length = String.length s in
    if i >= length then ""
    else (* first determine the width of the current line *)
      let current_width = 
        if min_width <> 0 && length mod min_width = 0 && length mod max_width < min_width && length mod max_width <> 0 && (length - i) <= min_width then min_width
        else if countdown length max_width min_width 0 <> 0 && length - i > min_width then (max_width - countdown length max_width min_width 0)
        else if i + max_width <= length then max_width
        else (length - i) in
      let current_line = String.sub s i current_width in (* stores the sub string with the necessary width *)
        if i + current_width < length then current_line ^ "\n" ^ split s min_width max_width (i + current_width)
        else if current_width >= min_width then current_line (* return remaining characters *)
        else if (length - i) <= min_width then current_line (* if impossible, return last letters anyway *)
        else split s min_width max_width (i + current_width);;

let block_text (s : string) (min_width : int) (max_width : int) : string =
  if max_width <= 0 || String.length s <= 0 then ""
  else if min_width > max_width then split s 0 max_width 0
  else if String.length s > min_width then split s min_width max_width 0 (* test if it is actually more than the min_width *)
  else s;;
  
  let () = print_string (block_text "ABCDEFGHIJKLM" 3 12); print_newline (); print_newline ();;

