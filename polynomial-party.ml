(* Task: A polynomial is represented by the list [cn;cn−1;. . . ;c2;c1;c0] of its coefficients.
Implement eval_poly : float -> float list -> float that evaluates the polynomial (2. argument) for a given x (1. argument). 
Implement derive_poly : float list -> float list that returns the first derivative of the given polynomial.  *)

let eval_poly x coeffs =
  let rec impl value coeffs =
    match coeffs with [] -> value
    | c::cs -> impl ((value *. x) +. c) cs
  in
  impl 0.0 coeffs

let derive_poly coeffs = 
  let rec impl = function [] | [_] -> ([], 0.)
  | c::cs -> let (new_coeffs, deg) = impl cs in
    (c *. (deg +. 1.))::new_coeffs, deg +. 1.
  in fst (impl coeffs)

type test_input = { poly : float list; x : float }
let ex1 = { poly = [0.]; x = 3. }
let ex2 = { poly = [1.]; x = 8. }
let ex3 = { poly = [1.;0.]; x = -14. }
let ex4 = { poly = [1.;0.;1.;0.]; x = -3.5 }
let ex5 = { poly = [2.;-1.;8.]; x = 10.8 }
let ex6 = { poly = [23.;-103.;13.;1.;0.;0.;52.]; x = 2.2 }
