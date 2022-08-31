(*Task: Consider the following function foo implemented in an imperative programming language:
	int foo(int x, int y, bool b) {
		if(x > y) {
			int t = x;
			x = y;
			y = t;
		}
		while(x < y) {
			if(b) {
				++x;
			} else {
			--y;
			}
			b = !b;
		}
		return x;
	}

A semantically equivalent function in Ocaml looks like this: *)

let foo x y b =
  let x,y = if x > y then y,x else x,y in
  let rec loop x y b =
    if x >= y then x
    else if b then loop (x+1) y (not b)
    else loop x (y-1) (not b)
  in
  loop x y b


type test_input = { x : int; y : int; b : bool }
let ex1 = { x = 0; y = 0; b = false }
let ex2 = { x = 3; y = 18; b = true }
let ex3 = { x = 22; y = -4; b = true }
let ex4 = { x = -100; y = -100; b = false }
let ex5 = { x = 7; y = 8; b = false }

