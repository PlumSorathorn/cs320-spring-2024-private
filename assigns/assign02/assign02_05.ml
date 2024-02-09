(* Listing Paths

   Implement a function `all_paths` which given

     len : a nonnegative integer
     stp : a point (see below)
     endp : a point

   returns a list of `(dir * int)` lists, where each list contains
   sequence of directions to get from `stp` to `endp` in exactly `len`
   steps.

   Notes:
   A sequence of directions is a list of `dir * int` pairs with the
   property that no adjacent elements have the same `dir`.  For example,

     [(N, 1); (S, 1); (N, 3)]

   is a valid sequence of directions but

     [(N, 1); (N, 3); (S, 1)]

   is not. Instead, this should be

     [(N, 4), (S, 1)]

   Examples:
   let origin = {x=0;y=0}
   let _ = assert (all_paths 0 origin origin = [[]])
   let _ = assert (all_paths 1 origin origin = [])
   let _ = assert (all_paths 2 origin origin =
       [[(N,1);(S,1)] ;
        [(S,1);(N,1)] ;
        [(E,1);(W,1)] ;
        [(W,1);(E,1)] ])
   let _ = assert (all_paths 3 origin origin = [])
   let _ = assert (List.mem [(N,2);(S,2)] (all_paths 4 origin origin))
   let _ = assert (List.mem [(N,1);(E,1);(W,1);(S,1)] (all_paths 4 origin origin))

*)

type dir = N | S | E | W

type point = {
  x : int ;
  y : int ;
}

let rec dist x y =
  if x > y then [] else x :: dist (x + 1) y

let rec append_lists lists = match lists with
  | [] -> []
  | n :: rest -> n @ append_lists rest

let rec transform func list = match list with
  | [] -> []
  | n :: rest -> (func n) :: (transform func rest)

let move_point cur_point dir n = match dir with
  | N -> { cur_point with y = cur_point.y + n }
  | S -> { cur_point with y = cur_point.y - n }
  | E -> { cur_point with x = cur_point.x + n }
  | W -> { cur_point with x = cur_point.x - n }

let all_paths len stp endp =
  let rec dfs stp endp len path last_dir =
    if len = 0 then
      if stp = endp then [List.rev path] else []
    else
      append_lists (transform (fun dir ->
        if Some dir = last_dir then []
        else
          append_lists (transform (fun steps ->
            if steps > len then []
            else
              let next_point = move_point stp dir steps in
              let new_path = (dir, steps) :: path in
              dfs next_point endp (len - steps) new_path (Some dir)
          ) (dist 1 len))
      ) [N; S; E; W])
    in dfs stp endp len [] None
