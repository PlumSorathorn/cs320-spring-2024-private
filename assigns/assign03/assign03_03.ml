(* Boolean Expressions

   Implement a function `eval` which given

     v: an association list mapping `string`s to `bool`s
     e: a boolean expression (see below)

   returns `Some b` if every variable in `e` also appears in `v` and
   `e` evaluates to `b`, or `None` if not all variable in `e` appear
   in `v`.

   Example:
   let v = [("a", true); ("b", false); ("c", true)]
   let e = And (Var "a", Or (Var "b", Var "c"))
   let f = Not (Var "d")
   let _ = assert (eval v e = Some true)
   let _ = assert (eval v f = None)

   One way to think about evaluation: imagine `v` as defining a
   collection of `bools` in OCaml:

     let a = true
     let b = false
     let c = true

   and an expression as defining a boolean expression in OCaml:

     let e = a && (b || c)

   The goal of evaluation is to determine the value of `e`.

   Likewise, if you define an expression with a name that has not
   been defined, you would get a compile-time error

     let f = not d

   which is why the function `eval` should return `None` on `f`.

   Hint: Take a look at the textbook section on association lists
   (they are a simple implementation of a dictionary-like data
   structure), as well as the function List.assoc_opt.

*)


type bexp =
  | Var of string
  | Not of bexp
  | And of bexp * bexp
  | Or of bexp * bexp

let eval (v : (string * bool) list) (e : bexp) : bool option =
  let rec loop_eval vars exp = 
    match vars, exp with
    | (a, b) :: rest, Var x -> 
      if a = x && b then Some true 
      else if a = x && not b then Some false
      else if rest <> [] then loop_eval rest (Var x)
      else None
    | (a, b) :: rest, Not x -> 
      if ((loop_eval vars x) =  Some true) then Some false
      else if ((loop_eval vars x) =  Some false) then Some true
      else if rest <> [] then loop_eval rest x
      else None
    | (a, b) :: rest, And (x, y) -> 
      if ((loop_eval vars x) =  Some true) && ((loop_eval vars y) = Some true) then Some true
      else if ((loop_eval vars x) =  Some false) || ((loop_eval vars y) = Some false) then Some false
      else if rest <> [] then loop_eval rest x
      else None
    | (a, b) :: rest, Or (x, y) ->
      if ((loop_eval vars x) =  Some true) || ((loop_eval vars y) = Some true) then Some true
      else if ((loop_eval vars x) =  Some false) && ((loop_eval vars y) = Some false) then Some false
      else if rest <> [] then loop_eval rest x
      else None
    | _,_ -> None
  in loop_eval v e;;


