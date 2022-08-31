(* The natural numbers can be defined recursively as follows:
- 0 is a natural number.
- if n is a natural number, then so is the successor of n

This can be represented in Ocaml as follows: *)

type nat = Zero | Succ of nat

(*some functions to work with natural numbers*)
let rec int_to_nat i = if i <= 0 then Zero else Succ (int_to_nat (i-1))

let rec nat_to_int n = match n with Zero -> 0 | Succ x -> 1 + nat_to_int x

let rec add x y = match x with Zero -> y | Succ x' -> add x' (Succ y)

let rec mul x y = match x with Succ x' -> add y (mul x' y) | Zero -> Zero

let rec pow x y = match y with Zero -> Succ Zero | Succ y' -> mul x (pow x y')

let rec leq x y = match x,y with Succ x', Succ y' -> leq x' y' | _ -> x = Zero
