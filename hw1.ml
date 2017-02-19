type peano = Z | S of peano;;
type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;

let rec peano_of_int x = if (x == 0) then  Z else S (peano_of_int (x - 1));;

let rec int_of_peano p = match p with
		Z -> 0
		| S x -> 1 + int_of_peano x;;

let inc x = S x;;

let rec add x y = match y with 
		Z -> x 
		| S yy -> S(add x yy);;

let rec mul x y =  match y with
		Z -> Z
		| S yy -> add (mul x yy) x;;

let rec sub x y = match (x, y) with
		(Z, yy) -> Z
		| (xx, Z) -> xx
		| (S xx, S yy) -> sub xx yy;;

let rec power x y = match y with
		Z -> S Z
		| S yy -> mul (power x yy) x;;

let rec print_peano x = match x with 
		Z -> print_string "\n"; print_int 0
		| S xx -> print_peano xx; print_string "\'";;
                     
let rev x = failwith "Not implemented";;
let merge_sort x = failwith "Not implemented";;
                     
let string_of_lambda x = failwith "Not implemented";;
let lambda_of_string x = failwith "Not implemented";;