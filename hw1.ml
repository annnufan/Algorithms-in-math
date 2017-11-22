type peano = Z 
	| S of peano;;

type lambda = Var of string 
	| Abs of string * lambda 
	| App of lambda * lambda;;
                     
let rec peano_of_int x = match x with
	0 -> Z 
	| _ -> S (peano_of_int (x - 1));;

let rec int_of_peano p = match p with
	Z -> 0
	| S x -> 1 + int_of_peano x;;

let inc x = S x;;

let rec add x y = match y with 
	Z -> x 
	| S yy -> S(add x yy);;

let rec mul x y = match y with
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

let rec reverse x y = match x with
	[] -> y
	| n::xx -> reverse xx (n::y);;
                     
let rev x = reverse x [];;

let rec size x = match x with
	[] -> 0
	| y::xx -> (size xx) + 1;;

let rec merge x y = match x with
	[] -> y
	| head::tail -> match y with
		[] -> x
		| head1::tail1 -> if head < head1 then
				head::(merge tail y)
			else
				head1::(merge x tail);;

let rec take x left right = match x with
	[] -> (left, right)
	| head::tail -> take tail right (head::left);; 

let rec merge_sort x = match x with
		[] -> []
		| n::xx -> let (head, tail) = take x [] [] in
			merge (merge_sort head) (merge_sort tail);;

let rec string_of_lambda str = match str with 
	Var x -> x
	| Abs(x, lmb) -> "(\\" ^ x ^ "." ^ string_of_lambda lmb ^ ")"
	| App(lmb1, lmb2) -> "(" ^ string_of_lambda lmb1 ^ " " ^ string_of_lambda lmb2 ^ ")";; 

let is_var str = (('a' <= str) && (str <= 'z')) || (('0' <= str) && (str <= '9')) || (str = '\'');;

let iter = ref 0;;

let rec get_var str ans = match str.[!iter] with 
	a when is_var a -> iter := !iter + 1; get_var str (ans ^ (String.make 1 a))
	| '.' | ' ' | ')' | ';' -> ans 
	| _ -> failwith "Unexpected symbol";;

let parse_var x = get_var x "";;

let rec parse_expr str flg = match str.[!iter] with
	'\\' -> iter := !iter + 1; let a = parse_var str in Abs (a, parse_expr str true)
	| '.' -> iter := !iter + 1; parse_expr str true
	| '(' -> iter := !iter + 1; let a = parse_expr str true in
									if str.[!iter] <> ')' then failwith "Missing \")\""
									else (
										iter := !iter + 1;
										if flg then parse_app str a else a
									)
	| ';' -> failwith "Expression ended unexpectedly"
	| _  -> if flg then 
				parse_app str (Var (parse_var str))
			else
				Var (parse_var str)
	and
	parse_app str ans = match str.[!iter] with 
	' ' -> (iter := !iter + 1; parse_app str (App (ans, (parse_expr str false))))
	| _  -> ans;;

let rec parse_app str ans = match str.[!iter] with 
	' ' -> (iter := !iter + 1; parse_app str (App (ans, (parse_expr str false))))
	| _  -> ans;;

let rec lambda_of_string x = 
	let x = x ^ ";" in iter := 0; parse_expr x true;;