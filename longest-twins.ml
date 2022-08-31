(* Task: Implement a function lt_seq : 'a list -> 'a list that returns the longest sequence
of elements that appears at least twice in the given list. If multiple sequences of the same
length exist, the one that appears first shall be returned. The sequences must not overlap.*)

let lt_seq l = 
  (* compare the first up to n elements of l1 and l2 to find the longest common prefix, not longer than n *)
  let rec longest_prefix n l1 l2 =
    if n <= 0 then [], 0 else
    match l1, l2 with x::xs, y::ys ->
      if x <> y then [], 0 else
      let l,n = longest_prefix (n-1) xs ys in
      (x::l, n+1)
    | _ -> [], 0
  in
  (* iterate through the list l2 and compare it with l1 *)
  let rec iter_l2 n l1 l2 (best_l, best_n) =
    match l2 with [] -> (best_l, best_n)
    | y::ys -> let pre_l, pre_n = longest_prefix n l1 l2 in
      iter_l2 (n+1) l1 ys (if pre_n > best_n then (pre_l, pre_n) else (best_l, best_n))
  in
  (* iterate through the list l1 *)
  let rec iter_l1 l1 (best_l, best_n) =
    match l1 with [] -> best_l
    | x::xs -> iter_l1 xs (iter_l2 1 l1 xs (best_l, best_n))
  in iter_l1 l ([], 0)


(* example inputs *)
let ex1 = [1;2;2;3;4;2;2;2;3;1]
let ex2 = [true;false;false;true]
let ex3 = ['a';'a';'b';'b';'a';'b';'b';'a';'a']
let ex4 = [0.;1.;2.;0.;2.;1.;2.;1.;2.;3.]

