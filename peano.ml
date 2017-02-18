print_string "Hello, user.";;

type num = Z|S of num;;

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

let rec deg x y = match y with
					Z -> S Z
					| S yy -> mul (deg x yy) x;;


let y = S (S Z);;
let x = deg y y;;

let rec print_num x = match x with 
						Z -> print_string "\n"; print_int 0
						| S xx -> print_num xx; print_string "\'";;


print_num (mul x y);;

print_num (add x y);;

print_num (inc x);;

print_num (sub x y);;