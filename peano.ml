print_string "Hello, user.";;

type peano = Z | S of peano;;

let rec peano_of_int x = if (x == 0) then  Z else S (peano_of_int (x - 1));;

let rec int_of_peano p = match p with
	    Z -> 0
		| S xx -> 1 + int_of_peano xx;;

let inc x = S x;;

let rec add x y = match y with 
		Z -> x 
		| S yy -> S(add x yy);;

let rec sub x y = match (x, y) with
		(Z, yy) -> Z
		| (xx, Z) -> xx
		| (S xx, S yy) -> sub xx yy;;

let rec mul x y = match y with
		Z -> Z
		| S yy -> add (mul x yy) x;;

let rec power x y = match y with
		Z -> S Z
		| S yy -> mul (power x yy) x;;

let rec print_peano x = match x with 
		Z -> print_string "\n"; print_int 0
		| S xx -> print_num xx; print_string "\'";;