(* UTILITIES *)
let cons x xs = x :: xs
let explode s = List.of_seq (String.to_seq s)
let implode cs = String.of_seq (List.to_seq cs)
let is_digit c = '0' <= c && c <= '9'
let is_blank c = String.contains " \012\n\r\t" c
let is_upper_case c = 'A' <= c && c <= 'Z'
let is_lower_case c = 'a' <= c && c <= 'z'

type 'a parser = char list -> ('a * char list) option

let satisfy f = function
  | c :: cs when f c -> Some (c, cs)
  | _ -> None

let char c = satisfy ((=) c)

let str s cs =
  let rec go = function
    | [], ds -> Some (s, ds)
    | c :: cs, d :: ds when c = d -> go (cs, ds)
    | _ -> None
  in go (explode s, cs)

let map f p cs =
  match p cs with
  | Some (x, cs) -> Some (f x, cs)
  | None -> None

let (>|=) p f = map f p
let (>|) p x = map (fun _ -> x) p

let seq p1 p2 cs =
  match p1 cs with
  | Some (x, cs) -> (
      match p2 cs with
      | Some (y, cs) -> Some ((x, y), cs)
      | None -> None
    )
  | None -> None

let (<<) p1 p2 = map fst (seq p1 p2)
let (>>) p1 p2 = map snd (seq p1 p2)

let map2 f p1 p2 =
  seq p1 p2 >|= fun (x, y) -> f x y

let optional p cs =
  match p cs with
  | Some (x, cs) -> Some (Some x, cs)
  | None -> Some (None, cs)

let rec many p cs =
  match p cs with
  | Some (x, cs) -> (
      match (many p cs) with
      | Some (xs, cs) -> Some (x :: xs, cs)
      | None -> Some ([x], cs)
    )
  | None -> Some ([], cs)

let many1 p = map2 cons p (many p)

let alt p1 p2 cs =
  match p1 cs with
  | Some x -> Some x
  | None ->
    match p2 cs with
    | Some x -> Some x
    | None -> None

let (<|>) = alt

let pure x cs = Some (x, cs)
let fail _ = None

let bind p f cs =
  match p cs with
  | Some (x, cs) -> f x cs
  | None -> None

let (>>=) = bind
let ( let* ) = bind

let choice ps =
  List.fold_left (<|>) fail ps

let ws = many (satisfy is_blank)
let keyword w = str w << ws

let rec_parser p =
  pure () >>= p

let parse p s =
  match p (explode s) with
  | Some (x, []) -> Some x
  | _ -> None

(* END OF UTILITIES *)

(* REQUIRED TYPES *)

type ident = string

type const
  = Num of int
  | Bool of bool

type command
  = Push of const | Trace
  | Add | Mul | Div
  | And | Or | Not | Lt | Eq
  | If of program * program
  | While of program * program
  | Bind of ident | Fetch of ident
  | Fun of program | Call | Return
  | Debug of string

and program = command list

and bindings = (ident * value) list

and value
  = Const of const
  | Clos of
      { def_id : int
      ; captured : bindings
      ; prog : program
      }

type record =
  { id : int
  ; local : bindings
  ; called_def_id : int
  ; return_prog : program
  }

type stack = value list
type trace = string list
type env
  = Global of bindings
  | Local of record * env

(* get the id of the topmost record *)
let local_id = function
  | Global _ -> 0
  | Local (r, _) -> r.id

(* convert a value to a string *)
let to_string v =
  match v with
  | Const (Bool true) -> "True"
  | Const (Bool false) -> "False"
  | Const (Num n) -> string_of_int n
  | Clos _ -> "<Closure>"

(* PARSING *)

let parse_ident =
  map2
    (fun c cs -> implode (c :: cs))
    (satisfy is_lower_case)
    (many (satisfy (fun c -> is_lower_case c || is_digit c || c = '_')))

let parse_int =
  let mk_int sign cs =
    let abs = int_of_string (implode cs) in
    if Option.is_none sign
    then abs
    else -abs
  in
  map2
    mk_int
    (optional (char '-'))
    (many1 (satisfy is_digit))

let parse_bool =
  (str "True" >| true) <|> (str "False" >| false)

let parse_comment =
  char '(' >> many (satisfy ((<>) ')')) >> char ')' >| ()

let parse_debug =
  char '"' >> many (satisfy ((<>) '"')) << char '"' >|= implode

let ws = many (ws >> parse_comment) >> ws
let keyword w = str w << ws

let rec parse_com () =
  let parse_fun = 
    let* _ = keyword ":" in 
    let* fun_name = parse_prog_rec () in 
    let* _ = keyword ";" in 
    pure (Fun (fun_name))
  in
  let parse_if =
    let* _ = keyword "?" in
    let* ifc = parse_prog_rec () in
    let* _ = keyword ";" in
    let* elsec = parse_prog_rec () in
    let* _ = char ';' in
    pure (If (ifc, elsec))
  in
  let parse_while =
    let* _ = keyword "While" in
    let* check = parse_prog_rec () in
    let* _ = keyword ";" in
    let* body = parse_prog_rec () in
    let* _ = char ';' in
    pure (While (check, body))
  in
  let parse_trace = 
    let* _ = keyword "." in 
    pure (Trace)
  in 
  let parse_return = 
    let* _ = keyword "Return" in 
    pure (Return)
  in
  let parse_call = 
    let* _ = keyword "#" in 
    pure (Call)
  in
  let parse_bind = 
    let* _ = keyword "|>" in 
    let* body = parse_ident in 
    pure (Bind (body))
  in
  let parse_equal = 
    let* _ = keyword "=" in 
    pure (Eq)
  in
  let parse_less_than = 
    let* _ = keyword "<" in 
    pure (Lt)
  in 
  let parse_not = 
    let* _ = keyword "~" in
    pure (Not)
  in 
  let parse_or = 
    let* _ = keyword "||" in 
    pure (Or)
  in 
  let parse_and = 
    let* _ = keyword "&&" in 
    pure (And)
  in 
  let parse_divide = 
    let* _ = keyword "/" in 
    pure (Div)
  in 
  let parse_multiply = 
    let* _ = keyword "*" in 
    pure (Mul)
  in 
  let parse_add = 
    let* _ = keyword "+" in 
    pure (Add)
  in 
  let parse_const = 
    let* const = (parse_bool >|= fun c -> Bool c) <|> (parse_int >|= fun c -> Num c)  in
    pure (Push (const))
  in 
  choice
    (* TODO: Add more alternatives *)
    [ parse_fun
    ; parse_while
    ; parse_if
    ; parse_ident >|= (fun s -> Fetch s)
    ; parse_debug >|= (fun s -> Debug s)
    ; parse_trace
    ; parse_return 
    ; parse_call
    ; parse_bind
    ; parse_equal 
    ; parse_less_than 
    ; parse_not
    ; parse_or 
    ; parse_and 
    ; parse_divide 
    ; parse_multiply 
    ; parse_add 
    ; parse_const
    ]
and parse_prog_rec () =
  many (rec_parser parse_com << ws)

let parse_prog = parse (ws >> parse_prog_rec ())

(* FETCHING AND UPDATING *)

(* fetch the value of `x` in the environment `e` *)
let rec fetch_env (e : env) (x : ident) = (* TODO *)
  let rec find_binding (bs : bindings) (x : ident) : value option = 
    match bs with 
    | [] -> None 
    | (id, value) :: rest -> if id = x then Some value 
    else find_binding rest x
  in
  match e with 
  | Global b -> find_binding b x
  | Local (r, outer_env) ->
    match find_binding r.local x with 
    | Some value -> Some value 
    | None -> fetch_env outer_env x

let rec update_env (e : env) (x : ident) (v : value) : env = (* TODO *)
  match e with
  | Global b -> Global ((x, v) :: b)
  | Local (r, outer_env) ->
    if List.mem_assoc x r.local then 
      Local ({r with local = (x, v) :: r.local }, outer_env)
  else
    Local (r, update_env outer_env x v)
    
(* EVALUTION *)

(* make the panic configuration given a configuration *)
let panic (_, _, t, _) msg = [], Global [], ("panic: " ^ msg) :: t, []

let eval_step (c : stack * env * trace * program) =
  match c with
  (* Push *)
  | s, e, t, Push c :: p -> Const c :: s, e, t, p
  (* Trace *)
  | v :: s, e, t, Trace :: p -> s, e, to_string v :: t, p
  | [], _, _, Trace :: _ -> panic c "stack underflow (. on empty)"
  (* Add *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Add :: p -> Const (Num (m + n)) :: s, e, t, p
  | _ :: _ :: _, _, _, Add :: _ -> panic c "type error (+ on non-integers)"
  | _ :: [], _, _, Add :: _ -> panic c "stack underflow (+ on single)"
  | [], _, _, Add :: _ -> panic c "stack underflow (+ on empty)"
  (* TO DO *)(* Multiply *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Mul :: p -> Const (Num (m * n)) :: s, e, t, p
  | _ :: _ :: _, _, _, Mul :: _ -> panic c "type error (* on non-integers)"
  | _ :: [], _, _, Mul :: _ -> panic c "stack underflow (* on single)"
  | [], _, _, Mul :: _ -> panic c "stack underflow (* on empty)"
  (* Divide *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Div :: p when n <> 0 -> Const (Num (m / n)) :: s, e, t, p
  | Const (Num m) :: Const (Num n) :: s, e, t, Div :: p when n = 0 -> panic c "type error (/ by zero)"
  | _ :: _ :: _, _, _, Div :: _ -> panic c "type error (/ on non-integers)"
  | _ :: [], _, _, Div :: _ -> panic c "stack underflow (/ on single)"
  | [], _, _, Div :: _ -> panic c "stack underflow (/ on empty)"
  (* And *)
  | Const (Bool m) :: Const (Bool n) :: s, e, t, And :: p -> Const (Bool (m && n)) :: s, e, t, p
  | _ :: _ :: _, _, _, And :: _ -> panic c "type error (&& on non-booleans)"
  | _ :: [], _, _, And :: _ -> panic c "stack underflow (&& on single)"
  | [], _, _, And :: _ -> panic c "stack underflow (&& on empty)"
  (* Or *)
  | Const (Bool m) :: Const (Bool n) :: s, e, t, Or :: p  -> Const (Bool (m || n)) :: s, e, t, p
  | _ :: _ :: _, _, _, Or :: _ -> panic c "type error (|| on non-booleans)"
  | _ :: [], _, _, Or :: _ -> panic c "stack underflow (|| on single)"
  | [], _, _, Or :: _ -> panic c "stack underflow (|| on empty)"
  (* Not *)
  | Const (Bool m) :: s, e, t, Not :: p -> Const (Bool (not m)) :: s, e, t, p
  | _ :: _, _, _, Not :: _ -> panic c "type error (~ on non-booleans)"
  | [], _, _, Not :: _ -> panic c "stack underflow (~ on empty)"
  (* Less than *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Lt :: p -> 
    if m < n 
      then Const (Bool true) :: s, e, t, p
    else
      Const (Bool false) :: s, e, t, p
  | _ :: _ :: _, _, _, Lt :: _ -> panic c "type error (< on non-integers)"
  | _ :: [], _, _, Lt :: _ -> panic c "stack underflow (< on single)"
  | [], _, _, Lt :: _ -> panic c "stack underflow (< on empty)"
  (* Equals *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Eq :: p -> 
    if m = n
      then Const (Bool true) :: s, e, t, p
    else
      Const (Bool false) :: s, e, t, p
  | _ :: _ :: _, _, _, Eq :: _ -> panic c "type error (= on non-integers)"
  | _ :: [], _, _, Eq :: _ -> panic c "stack underflow (= on single)"
  | [], _, _, Eq :: _ -> panic c "stack underflow (= on empty)"
  (* If-Else *)
  | Const (Bool true) :: s, e, t, If (m, n) :: p -> s, e, t, m @ p
  | Const (Bool false) :: s, e, t, If (m, n) :: p -> s, e, t, n @ p
  | _ :: _, _, _, If (_, _) :: _ -> panic c "type error (? on non-booleans)"
  | [], _, _, If (_, _) :: _ -> panic c "stack underflow (? on empty)"
  (* While *)
  | s, e, t, While(m, n) :: p -> s, e, t, m @ (If(n, []) :: p)
  (* Fetch *)
  | s, e, t, Fetch n :: p -> (
    match (fetch_env e n) with 
    | Some x -> x :: s, e, t, p
    | None -> panic c "fetch failed"
  )
  (* Assign *)
  | v :: s, e, t, Bind n :: p -> s, (update_env e n v), t, p
  | [], _, _, Bind n :: p -> panic c "stack underflow (assign on empty)"
  (* Call *)
  | Clos {def_id; captured; prog} :: s, e, t, Call :: p -> 
    let new_r = {id = (local_id e) + 1; local = captured; called_def_id = def_id; return_prog = p } in
    s, Local (new_r, e), t, prog
  | _ :: s, e, t, Call :: p -> panic c "type error (call on non-closure)"
  | [], _, _, Call :: _ -> panic c "stack underflow (call on empty)"
  (* Return *)
  | Clos {def_id; captured; prog} :: [], Local ({id; local; called_def_id; return_prog}, e), t, Return :: p when def_id = id -> 
    let merged_b = captured @ local in 
    Clos {def_id = id; captured = merged_b; prog = prog} :: [], e, t, return_prog
  | Clos {def_id; captured; prog} as v :: [], Local ({id; local; called_def_id; return_prog}, e), t, Return :: p when def_id <> id ->
    v :: [], e, t, return_prog
  | x :: [], Local ({id; local; called_def_id; return_prog}, e), t, Return :: p -> x :: [], e, t, return_prog
  | [], Local ({id; local; called_def_id; return_prog}, e), t, Return :: p -> [], e, t, return_prog
  | [], Local ({id; local; called_def_id; return_prog}, e), t, [] -> [], e, t, return_prog
  | x :: y :: s, Local ({id; local; called_def_id; return_prog}, e), t, Return :: p -> panic c "return error (more than one value)"
  | x :: s, Local ({id; local; called_def_id; return_prog}, e), t, [] -> panic c "return error (stack not empty)"
  | s, Global (_), t, Return :: p -> panic c "return error (global)"
  (* Function *)
  | s, e, t, Fun (m) :: p -> 
    let closure = Clos {def_id = local_id e; captured = []; prog = m} in 
    closure :: s, e, t, p
  (* Debug *)
  | s, e, t, Debug m :: p ->
    let _ = print_endline(m) in 
    s, e, t, p
  | _ -> assert false

let rec eval c =
  match c with
  | (_, Global _, t, []) -> t
  | _ -> eval (eval_step c)

let rec eval_prog p = eval ([], Global [], [], p)
let interp s = Option.map eval_prog (parse_prog s)

(* MAIN *)

let print_trace t =
  let rec go t =
    match t with
    | [] -> ()
    | x :: t ->
      print_endline x;
      go t
  in go (List.rev t)

let main () =
  let input =
    let rec get_input s =
      try
        get_input (s ^ "\n" ^ read_line ())
      with End_of_file ->
        s
    in get_input ""
  in
  match interp input with
  | None -> print_endline "Parse Error"
  | Some t -> print_trace t

let _ = main ()

(* END OF FILE *)
