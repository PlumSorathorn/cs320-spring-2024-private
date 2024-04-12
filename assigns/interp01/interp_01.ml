(* UTILITIES *)
let cons x xs = x :: xs
let explode s = List.of_seq (String.to_seq s)
let implode cs = String.of_seq (List.to_seq cs)
let is_digit c = '0' <= c && c <= '9'
let is_blank c = String.contains " \012\n\r\t" c
let is_upper_case c = 'A' <= c && c <= 'Z'

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

(* ============================================================ *)

(* BEGINNING OF PROJECT CODE *)

type ident = string
type command
  = Drop                   (* drop *)
  | Swap                   (* swap *)
  | Dup                    (* dup *)
  | Trace                  (* . *)
  | Add                    (* + *)
  | Sub                    (* - *)
  | Mul                    (* * *)
  | Div                    (* / *)
  | Lt                     (* < *)
  | Eq                     (* = *)
  | Bind of ident          (* |> ID *)
  | Call of ident          (* # ID *)
  | If of program          (* ? prog ; *)
  | Def of ident * program (* def prog ; *)
  | Ident of ident         (* ID *)
  | Num of int             (* num *)
and program = command list

let parse_ident : ident parser = (* TODO *)
  let letter = satisfy is_upper_case in 
  many1 letter >|= implode


(* helper to parse num *)
let parse_number : int parser =
  let digit = satisfy is_digit >|= fun d -> int_of_char d - int_of_char '0' in
  many1 digit >>= fun digits ->
  pure (List.fold_left (fun a b -> a * 10 + b) 0 digits)


(* You are not required to used this but it may be useful in
   understanding how to use `rec_parser` *)
let rec parse_com ()  =
  let parse_def =
    map2
      (fun id p -> Def (id, p))
      (keyword "def" >> parse_ident << ws)
      (parse_prog_rec () << char ';')
  in parse_def <|> (* TODO *)
  let parse_if = 
    map
      (fun p -> If (p))
      ((keyword "?") >> parse_prog_rec () << char ';')
  in parse_if <|> 
  let parse_call = 
    (char '#' >> parse_ident) >|= fun id -> Call id
  in parse_call <|>
  let parse_bind = 
    (keyword "|>" >> parse_ident) >|= fun id -> Bind id
  in parse_bind <|>
  let parse_eq = 
    (char '=') >|= fun eq -> Eq
  in parse_eq <|> 
  let parse_lt = 
    (char '<') >|= fun lt -> Lt
  in parse_lt <|>
  let parse_div = 
    (char '/') >|= fun div -> Div
  in parse_div <|>
  let parse_mul = 
    (char '*') >|= fun mul -> Mul
  in parse_mul <|>
  let parse_sub = 
    (char '-') >|= fun sub -> Sub
  in parse_sub <|>
  let parse_add = 
    (char '+') >|= fun add -> Add
  in parse_add <|> 
  let parse_trace = 
    (char '.') >|= fun trace -> Trace
  in parse_trace <|>
  let parse_dup = 
    (keyword "dup") >|= fun dup -> Dup
  in parse_dup <|>
  let parse_swap = 
    (keyword "swap") >|= fun swap -> Swap
  in parse_swap <|> 
  let parse_drop = 
    (keyword "drop") >|= fun drop -> Drop
  in parse_drop <|>
  let parse_num = 
    (parse_number << ws) >|= fun num -> Num num
  in parse_num <|> 
  let parse_id = 
    (parse_ident << ws >|= fun id -> Ident id)
  in parse_id
and parse_prog_rec () =
  many ((rec_parser parse_com) << ws)

let parse_prog = (* TODO *)
  parse (ws >> parse_prog_rec () << ws)

(* A VERY SMALL TEST SET *)
let test = parse_prog "drop"
let out = Some [Drop]
let _ = assert (test = out)

let test = parse_prog "     .       "
let out = Some [Trace]
let _ = assert (test = out)

let test = parse_prog "  |> TEST   "
let out = Some [Bind "TEST"]
let _ = assert (test = out)

let test = parse_prog "  23 00345 + |> OK "
let out = Some [Num 23; Num 345; Add; Bind "OK"]
let _ = assert (test = out)

let test = parse_prog "  def NEG 0 - ; 2 #NEG 2 =    \n\n   "
let out = Some [Def ("NEG", [Num 0; Sub]); Num 2; Call "NEG"; Num 2; Eq]
let _ = assert (test = out)

let test = parse_prog "
  def ABS
    dup 0 swap < ?
      0 -
    ;
  ;

  30 0 -
  #ABS
  |> X
"
let out = Some
    [ Def ("ABS", [Dup; Num 0; Swap; Lt; If [Num 0; Sub]])
    ; Num 30; Num 0; Sub
    ;  Call "ABS"
    ;  Bind "X"
    ]
let _ = assert (test = out)


(* EVALUATION *)

type stack = int list
type value
  = Num of int
  | Prog of program
type env = (ident * value) list
type trace = string list

let update_env = assert false (* TODO *)
let fetch_env = assert false (* TODO *)
let eval_prog = assert false (* TODO *)
let interp = assert false (* TODO *)

(* END OF PROJECT CODE *)

(* ============================================================ *)

(* UNCOMMENT TO RUN INTERPRETER *)

let print_trace t =
  let rec go t =
    match t with
    | [] -> ()
    | x :: t ->
      print_endline x;
      go t
  in go (List.rev t)

(*
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
*)
