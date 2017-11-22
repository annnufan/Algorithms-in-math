type algebraic_term = Var of string | Fun of string * (algebraic_term list);;

(*--------------------------------------------------   System to equation   ---------------------------------------------------*)

let rec get alpha = match alpha with
	Var x -> x
	| Fun (f, lst) -> f ^ (List.fold_left (^) "" (List.map get lst));;

let gen_name var = let name_vars = List.map (fun (term1, term2) -> get term1 ^ get term2) var in
	"F" ^ (List.fold_left (^) "" name_vars);; 

(* По списку уравнений вернуть одно уравнение *)
let system_to_equation system = 
	let new_name = gen_name system in
	let (term1, term2) = List.split system in
	(Fun(new_name, term1), Fun(new_name, term2));;

(*------------------------------------------------------   Apply subst   -------------------------------------------------------*)

(* Применить подстановку к уравнению *)
let rec apply_substitution system var = match var with
	Var x -> (try let (f, y) = List.find (fun (term, eq) -> term = x) system in y with Not_found -> var)
	| Fun (f, lst) -> Fun (f, List.map (fun arg -> apply_substitution system arg)  lst);; 

(*----------------------------------------------------   Check solution   -----------------------------------------------------*)

let rec check_equals eq1 eq2 = match (eq1, eq2) with
	(Var x, Var y) -> x = y
	| (Fun(f, x), Fun(g, y)) -> f = g && List.for_all2 check_equals x y
	| _ -> false;;

(* Проверить решение *)
let check_solution eq1 eq2 = List.for_all (fun (term1, term2) -> check_equals (apply_substitution eq1 term1) (apply_substitution eq1 term2)) eq2;; 

(*-----------------------------------------------------   Solve system   ------------------------------------------------------*)

exception Not_solved;;

let rec check_used var eq= match eq with
	Var x -> x = var
	| Fun (f, x) -> List.exists (check_used var) x;;

let map_terms var term  = fun (x, y) -> (apply_substitution [(var, term)] x, apply_substitution [(var, term)] y);;

let rec unify lst1 lst2 = match lst1 with
	[] -> List.map (fun (term1, term2) -> match (term1, term2) with 
			(Var x, _) -> (x, term2) 
			| _ -> failwith "Error in unify") lst2
	| (term1, term2) :: term -> if check_equals term1 term2 then 
			unify term lst2
		else
			match (term1, term2) with
				(Var x, _) ->  if check_used x term2 then 
						raise Not_solved
					else
						unify (List.map (map_terms x term2) lst1) ((term1, term2) :: (List.map (map_terms x term2) lst2))
				| (Fun(f, x), Var y) -> unify ((term2, term1) :: term) lst2
				| (Fun(f, x), Fun(g, y)) -> if f = g then 
						(try let decomposed = List.combine x y in unify (decomposed @ term) lst2
				with Invalid_argument msg -> raise Not_solved)
			else raise Not_solved;;



(* Решить систему; если решения нет -- вернуть None *)
let solve_system system = (try Some (unify system []) with Not_solved -> None);;