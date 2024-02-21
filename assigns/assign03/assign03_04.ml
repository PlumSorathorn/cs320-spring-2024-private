(* Matrices

   In this problem you will be building a (very) small interface for
   matrices.  A matrix is represented as a record which keeps track of
   its numbers of rows and columns as well as the values represented
   as a list of rows (i.e., a list of lists).  You will be
   implementing some error handling by working with `result`s.

   ************

   Task 1: Implement a function `mkMatrix` which given

     rs : a list of lists

   returns a matrix represented by this list of lists if it is valid
   or an `error` otherwise.  The error conditions are:

   * If the lengths of the rows in `rs` are not all equal then
   `mkMatrix` should return `Error UnevenRows`.

   * If `rs` is empty then `mkMatrix` should return `Error ZeroRows`.

   * If `rs` contains only empty lists, then `mkMatrix` should reutrn
   `Error ZeroCols`.

   In other words, `mkMatrix` should only return a matrix if `rs`
   represents a nonempty rectangular grid of values.

   Example:
   let l = [[1;2;3];[4;5;6]]
   let rm = mkMatrix l
   let _ = match rm with
     | Ok m ->
       let _ = assert (m.num_rows = 2) in
       let _ = assert (m.num_cols = 3) in
       ()
     | _ -> assert false

   let r = [[1;2;3;4];[1;2;3]]
   let rm' = mkMatrix r
   let _ = match rm' with
     | Ok _ -> assert false
     | Error e -> assert (e = UnevenRows)

   ************

   Task 2: Implement the function `transpose` which, given

     m : a matrix

   returns the transpose of `m`.

   Example:
   let _ = match rm with
     | Ok m ->
       let tm = transpose m in
       let _ = assert (tm.num_rows = 3) in
       let _ = assert (tm.num_cols = 2) in
       let _ = assert (tm.rows = [[1;4];[2;5];[3;6]]) in
       ()
     | _ -> assert false

   ************

   Task 3: Implement the function `multiply` which, given

     m : a matrix
     n : a matrix

   returns the product of `m` and `n` if it is possible to multiply
   them. The error condition:

   * If the number of columns of `m` is not the same as the number of
   rows of `n`, then `multiply` should return `Error MulMismatch`

   Example:
   let a =
     { num_rows = 2 ;
       num_cols = 2 ;
       rows = [[1.;2.];[3.;4.]] ;
     }

   let b =
     { num_rows = 1 ;
       num_cols = 2 ;
       rows = [[1.; 2.]] ;
     }

   let _ = assert (multiply a a = Ok {
     num_rows = 2 ;
     num_cols = 2 ;
     rows = [[7.;10.];[15.;22.]] ;
    })

   let _ = assert (multiply a b = Error MulMismatch)

   ************

   References:
   https://en.wikipedia.org/wiki/Matrix_multiplication
   https://en.wikipedia.org/wiki/Transpose
   https://www.cs.bu.edu/fac/crovella/cs132-book/landing-page.html

*)

type error
   = UnevenRows
   | ZeroRows
   | ZeroCols
   | MulMismatch

type 'a matrix = {
  num_rows : int ;
  num_cols : int ;
  rows : ('a list) list ;
}

(* helper functions *)

let rec length lst =
  match lst with
  | [] -> 0
  | n :: rest -> 1 + length rest;;

let rec get_i lst index =
  match lst with
  | [] -> assert false
  | head :: tail -> if index = 0 then head else get_i tail (index - 1);;
(* end of helper functions *)
  
let mkMatrix (rs : 'a list list) : ('a matrix, error) result =
  let rec count_i lst acc = 
    match lst with
    | [] -> acc
    | n :: rest -> count_i rest (acc + 1) 
  in let rec same_length lsts len = 
    match lsts with
    | [] -> true
    | n :: rest -> (count_i n 0 = len) && same_length rest len 
  in let rec create_matrix lsts = 
    match lsts with
    | [] -> Error ZeroRows
    | [[]] -> Error ZeroCols
    | n :: rest ->
      let n_length = count_i n 0 in
      if n_length = 0 then Error ZeroCols
      else if same_length rest n_length then Ok { num_rows = count_i lsts 0; num_cols = n_length; rows = lsts }
      else Error UnevenRows 
    in create_matrix rs;;

let transpose (m : 'a matrix) : 'a matrix =
  let rec transpose_col rows col_i =
    match rows with
    | [] -> []
    | r :: rest ->
      if col_i < length r then
        (get_i r col_i) :: transpose_col rest col_i
      else []
    in let rec transposer m col_index =
    if col_index < m.num_cols then
      transpose_col m.rows col_index :: transposer m (col_index + 1)
    else []
    in let t_rows = transposer m 0 
  in { num_rows = m.num_cols; num_cols = m.num_rows; rows = t_rows };;


let dot row col =
  let rec dot_loop r c acc =
    match r, c with
    | [], [] -> acc
    | r :: r_rest, c :: c_rest -> dot_loop r_rest c_rest (acc +. r *. c)
    | _, _ -> acc
  in dot_loop row col 0.0;;

let multiply (m : float matrix) (n : float matrix) : (float matrix, error) result =
  if m.num_cols <> n.num_rows then Error MulMismatch
  else
    let transposed_n = transpose n in
    let rec times_row m_row n_col =
      match m_row with
      | [] -> []
      | m_row :: m_rest ->
          let rec times_col n_columns =
            match n_columns with
            | [] -> []
            | n_col :: n_rest -> (dot m_row n_col) :: (times_col n_rest)
          in (times_col n_col) :: (times_row m_rest n_col)
      in let product_rows = (times_row m.rows transposed_n.rows) 
    in Ok { num_rows = m.num_rows; num_cols = n.num_cols; rows = product_rows };;
