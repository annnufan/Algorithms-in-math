open Hw1

module String_set = Set.Make (String);;
module String_map = Map.Make(String);;

type lambda_ref = Var_ref of string
	| Abs_ref of string * lambda_ref ref
	| App_ref of lambda_ref ref * lambda_ref ref;;

(*------------------------------------------------------   Free vars   -------------------------------------------------------*)

let rec find_free expr blocked_lst = match expr with
	Var x -> 
		if String_set.mem x blocked_lst then
			String_set.empty
		else
			String_set.singleton x
	| Abs(x, lmb) ->
		find_free lmb (String_set.add x blocked_lst)
	| App(lmb1, lmb2) ->
		String_set.union (find_free lmb1 blocked_lst) (find_free lmb2 blocked_lst);;

(* Вернуть список имён свободных переменных *)
let free_vars expr = 
	let set = find_free expr String_set.empty in
		String_set.elements set;;

(*----------------------------------------------------   Free to subst   ----------------------------------------------------*)

(* Проверить свободу для подстановки. Параметры:
   что подставляем, где подставляем, вместо чего подставляем *)
let rec free_to_subst what where var = match where with
	Var x -> true
	| Abs(x, lmb) -> x = var || ((not (List.mem x (free_vars what))) && free_to_subst what lmb var)
	| App(lmb1, lmb2) -> free_to_subst what lmb1 var && free_to_subst what lmb2 var;; 

(*---------------------------------------------------   Is normal form   ----------------------------------------------------*)

let rec no_exist_beta_redex lmb = match lmb with
	Var x -> true
	| App(Abs(x, lmb1), lmb2) -> false
	| App(lmb1, lmb2) -> no_exist_beta_redex lmb1 && no_exist_beta_redex lmb2
	| Abs(x, lmb) -> no_exist_beta_redex lmb;;


(* Проверить, находится ли лямбда-выражение в нормальной форме *)
let is_normal_form lmb = no_exist_beta_redex lmb;;

(*---------------------------------------------------   Is alpha equiv   ----------------------------------------------------*)

let counter = ref 0;;

let next_name () = 
	let name = "t" ^ string_of_int !counter in
	counter := !counter + 1;
	name;;

let rec subst what where var = match where with
	Var x -> if x = var then
			what
		else
			where
	| Abs(x, lmb) -> if x = var then
			where
		else 
			Abs(x, subst what lmb var)
	| App(lmb1, lmb2) -> App(subst what lmb1 var, subst what lmb2 var);;

(* Проверить, альфа-эквивалентны ли лямбда-выражения *)
let rec is_alpha_equivalent alpha beta = match alpha, beta with
	Var x, Var y -> x = y
	| Abs(x, lmb1), Abs(y, lmb2) -> 
		let t = Var (next_name()) in
		is_alpha_equivalent (subst t lmb1 x) (subst t lmb2 y)
	| App(lmb1, lmb2), App(lmb3, lmb4) -> is_alpha_equivalent lmb1 lmb3 && is_alpha_equivalent lmb2 lmb4
	| _ -> false;; 

(*--------------------------------------------------   Norm beta reduct   ---------------------------------------------------*)

let rec find_beta_redex alpha = if is_normal_form alpha then
		alpha
	else
		match alpha with
		Var x -> alpha
		| Abs(x, lmb) -> Abs(x, find_beta_redex lmb)
		| App(Abs(x, lmb1), lmb2) -> subst lmb2 lmb1 x
		| App(lmb1, lmb2) -> if is_normal_form lmb1 then 
				App(lmb1, find_beta_redex lmb2)
			else 
				App(find_beta_redex lmb1, lmb2);;

let rec rename alpha lst = match alpha with
	Var x -> (try String_map.find x lst with Not_found -> alpha)
	| Abs(x, lmb) -> let name = next_name() in Abs(name, rename lmb (String_map.add x (Var name) lst))
	| App(lmb1, lmb2) -> App(rename lmb1 lst, rename lmb2 lst);;

(* Выполнить один шаг бета-редукции, используя нормальный порядок *)
let normal_beta_reduction lmb = find_beta_redex (rename lmb String_map.empty);;

(*-------------------------------------------------   Reduce to norm form   --------------------------------------------------*)

let rec lambda_to_lambda_ref alpha = match alpha with 
	Var x -> ref (Var_ref x)
	| Abs(x, lmb) -> ref (Abs_ref(x, lambda_to_lambda_ref lmb))
	| App(lmb1, lmb2) -> ref (App_ref(lambda_to_lambda_ref lmb1, lambda_to_lambda_ref lmb2));;

let rec lambda_ref_to_lambda alpha = match !alpha with 
	Var_ref x -> Var x
	| Abs_ref(x, lmb) ->Abs(x, lambda_ref_to_lambda lmb)
	| App_ref(lmb1, lmb2) -> App(lambda_ref_to_lambda lmb1, lambda_ref_to_lambda lmb2);;

let rec copy alpha lst = match !alpha with 
	Var_ref x -> (try ref (String_map.find x lst) with _ -> ref (Var_ref x))
	| Abs_ref(x, lmb) -> let name = next_name() in ref (Abs_ref(name, copy lmb (String_map.add x (Var_ref name) lst)))
	| App_ref(lmb1, lmb2) -> ref (App_ref(copy lmb1 lst, copy lmb2 lst));;

let rec subst_ref what where var = match !where with
	Var_ref x -> if x = var then where := !what
	| Abs_ref(x, lmb) -> if x <> var then subst_ref what lmb var
	| App_ref(lmb1, lmb2) -> subst_ref what lmb1 var; subst_ref what lmb2 var;;

let rec redution alpha = match !alpha with
	Var_ref x -> false
	| Abs_ref(x, lmb) -> redution lmb
	| App_ref(lmb1, lmb2) -> match !lmb1 with
		Abs_ref(x, lmb) -> alpha := !(copy lmb String_map.empty); subst_ref lmb2 alpha x; true
		| _ -> redution lmb1 || redution lmb2;;

(* Свести выражение к нормальной форме с использованием нормального
   порядка редукции; реализация должна быть эффективной: использовать 
   мемоизацию *)
let reduce_to_normal_form alpha = 
	let alpha_ref = copy (lambda_to_lambda_ref alpha) String_map.empty in
	while redution alpha_ref do () done;
	lambda_ref_to_lambda alpha_ref;;