(* Grouping Integers and Strings

   Implement a function `convert` which given

     l : a list of `int_or_string`s

   returns a list of `int_list_or_string_list`s such that adjacent
   `int`s and `string`s are grouped together.

   Example:
   let test_in = [Int 2; Int 3; String "a"; String "b"; Int 4; String "c"]
   let test_out = [IntList [2;3]; StringList ["a";"b"]; IntList [4]; StringList ["c"]]
   let _ = assert (convert test_in = test_out)

*)

type int_list_or_string_list
  = IntList of int list
  | StringList of string list

type int_or_string
  = Int of int
  | String of string

let convert (l : int_or_string list) : int_list_or_string_list list =
  let rec list_concat e last_type (current_int_list : int list) (current_string_list : string list) : int_list_or_string_list list =
    match e, last_type with 
    | [], "int" -> [IntList current_int_list]
    | [], "string" -> [StringList current_string_list]
    | [], _ -> []
    | Int n :: rest, "start" -> list_concat rest "int" (current_int_list @ [n]) []
    | Int n :: rest, "int" -> list_concat rest "int" (current_int_list @ [n]) []
    | Int n :: rest, _ -> [StringList current_string_list] @ list_concat rest "int" (current_int_list @ [n]) []
    | String n :: rest, "start" -> list_concat rest "string" [] (current_string_list @ [n])
    | String n :: rest, "string" -> list_concat rest "string" [] (current_string_list @ [n])
    | String n :: rest, _ -> [IntList current_int_list] @ list_concat rest "string" [] (current_string_list @ [n])
  in list_concat l "start" [] [];;


