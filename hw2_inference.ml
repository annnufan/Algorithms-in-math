open Hw1
open Hw2_unify

module String_map = Map.Make(String);;

type simp_type = S_Elem of string 
	| S_Arrow of simp_type * simp_type;;

type hm_lambda = HM_Var of string 
	| HM_Abs of string * hm_lambda 
	| HM_App of hm_lambda * hm_lambda 
	| HM_Let of string * hm_lambda * hm_lambda;;

type hm_type = HM_Elem of string 
	| HM_Arrow of hm_type * hm_type 
	| HM_ForAll of string * hm_type;;

(*------------------------------------------------------   Infer type   -------------------------------------------------------*)

let counter = ref 0;;

let new_type () = 
	let name = "t" ^ string_of_int !counter in
	counter := !counter + 1;
	Var name;;

let rec delete_dub lst result = match lst with
	[] -> result
	| head :: tail -> if List.mem head result then delete_dub tail result else delete_dub tail (head :: result);;

let rec convert_sys alpha eq context goal = match alpha with
	Hw1.Var x -> (context, (let (_, e) = List.find (fun (t, e) -> t = x) context in e, goal) :: eq)
	| Hw1.Abs(x, lmb) -> let x_type = new_type() and lmb_type = new_type() in
		convert_sys lmb ((goal, Fun ("f", [x_type; lmb_type])) :: eq) ((x, x_type) :: context) lmb_type
	| Hw1.App(lmb1, lmb2) -> 
		let lmb2_type = new_type() in let lmb1_type = Fun ("f", [lmb2_type; goal]) in
		let (x, a) = (convert_sys lmb1 eq context lmb1_type) and 
			(y, b) = (convert_sys lmb2 eq context lmb2_type) in (x @ y, a @ b);;

let rec build_sys alpha = 
	let (sys, expr_type) = convert_sys alpha [] [] (new_type()) in
	(delete_dub sys [], delete_dub expr_type []);;

let rec type_of_term alpha = match alpha with
	Var x -> S_Elem x
	| Fun(f, left :: right :: []) -> S_Arrow(type_of_term left, type_of_term right)
	| _ -> failwith "unexpected algebraic term";;

let infer_simp_type alpha = 
	counter := 0;
	let (expr_type, sys) = build_sys alpha in
	match solve_system sys with
	None -> None
	| Some expr -> Some (List.map (fun (str, term) -> (str, type_of_term term)) (List.map (fun (str, term) -> (str, apply_substitution expr term)) expr_type), 
		let (_, tmp) = List.find (fun (str, term) -> str = "t" ^ "0") expr in type_of_term tmp);;



(*--------------------------------------------------------   Algo W   ---------------------------------------------------------*)

let new_name () = 
	let name = "x" ^ string_of_int !counter in
	counter := !counter + 1;
	name;;

let new_hm_type () = 
	let name = "t" ^ string_of_int !counter in
	counter := !counter + 1;
	HM_Elem name;;

exception Not_typed;;

let rec rename alpha vars = match alpha with
	HM_Var x -> (try String_map.find x vars with Not_found -> alpha)
	| HM_Abs (x, lmb) -> let x_type = new_name () in HM_Abs (x_type, rename lmb (String_map.add x (HM_Var x_type) vars))
	| HM_App (lmb1, lmb2) -> HM_App (rename lmb1 vars, rename lmb2 vars)
	| HM_Let (x, lmb1, lmb2) -> let x_type = new_name () in 
		let vars_upd = String_map.add x (HM_Var x_type) vars in 
		HM_Let (x_type, rename lmb1 vars_upd, rename lmb2 vars_upd);;

let rec get_context alpha free delete = match alpha with
	HM_Var x -> if not (List.mem x delete || String_map.mem x free) 
		then 
			String_map.add x (new_hm_type()) free 
		else 
			free
	| HM_Abs (x, lmb) -> get_context lmb free (x :: delete)
	| HM_App (lmb1, lmb2) -> String_map.fold (fun a b c -> String_map.add a b c) (get_context lmb1 free delete) (get_context lmb2 free delete)
	| HM_Let (x, lmb1, lmb2) -> String_map.fold (fun a b c -> String_map.add a b c) (get_context lmb1 free delete) (get_context lmb2 free (x :: delete));; 

let rec subst_vars alpha vars = match alpha with
	HM_Elem term ->  (try String_map.find term vars with Not_found -> alpha)
	| HM_Arrow (term1, term2) -> HM_Arrow (subst_vars term1 vars, subst_vars term2 vars)
	| HM_ForAll(_, _) -> failwith "Found internal quantifier";;

let rec rename_vars alpha vars = match alpha with
	HM_ForAll (x, term) -> rename_vars term (String_map.add x (new_hm_type()) vars)
	| _-> subst_vars alpha vars;;

let rec subst_to_hm_type alpha subst = match alpha with
	HM_Elem term -> (try let (var, beta) = List.find (fun (var1, theta) -> var1 = term) subst in beta with Not_found -> alpha)
	| HM_Arrow (term1, term2) -> HM_Arrow (subst_to_hm_type term1 subst, subst_to_hm_type term2 subst)
	| HM_ForAll (x, term) -> HM_ForAll (x, subst_to_hm_type term (List.filter (fun (var, beta) -> var <> x) subst));;

let rec alg_term_of_hm alpha = match alpha with
	HM_Elem x -> Var x
	| HM_Arrow (term1, term2) -> Fun ("arrow", [alg_term_of_hm term1; alg_term_of_hm term2])
	| HM_ForAll (x, term) -> Fun ("forall", [Var x; alg_term_of_hm term]);;

let rec hm_of_alg_term alpha = match alpha with
	Var x -> HM_Elem x
	| Fun (f, left :: right :: []) when f = "arrow" -> HM_Arrow (hm_of_alg_term left, hm_of_alg_term right)
	| Fun (f, Var x :: right :: []) when f = "forall" -> HM_ForAll (x, hm_of_alg_term right)
	| _ -> failwith "Unexpected algebraic term";;

let compose_for_two lst1 lst2 = let subst = List.map (fun (x, term) -> (x, subst_to_hm_type term lst1)) lst2 in
	let filtered = List.filter (fun (x, term1) -> (try let _ = List.find (fun (y, term2) -> x = y) lst1 in false with Not_found -> true)) subst in 
	lst1 @ filtered;;

let rec compose lst = match lst with
	| [] -> []
	| head :: [] -> head
	| head :: tail -> compose_for_two head (compose tail);;

let rec free_vars alpha free delete = match alpha with
	HM_Elem x -> if not (List.mem x delete || List.mem x free) then 
			x :: free 
		else 
			free
	| HM_ForAll (x, term) -> free_vars term free (x :: delete)
	| HM_Arrow (term1, term2) -> (free_vars term1 free delete) @ (free_vars term2 free delete);;

let rec infer_hm_type alpha context = match alpha with
	HM_Var x -> (try ([], rename_vars (String_map.find x context) String_map.empty) with Not_found -> raise Not_typed)
	| HM_Abs(x, lmb) -> let beta = new_hm_type() in
		let (subst, theta) = infer_hm_type lmb (String_map.add x beta context) in
		(subst, HM_Arrow (subst_to_hm_type beta subst, theta))
	| HM_App(lmb1, lmb2) -> let (subst1, term1) = infer_hm_type lmb1 context in
		let (subst2, term2) = infer_hm_type lmb2 (String_map.map (fun term -> subst_to_hm_type term subst1) context) and beta = new_hm_type() in
		(match solve_system [(alg_term_of_hm (subst_to_hm_type term1 subst2 ), alg_term_of_hm (HM_Arrow (term2, beta)))] with
			Some value -> let subst = List.map (fun (x, term) -> (x, hm_of_alg_term term)) value in
			(compose [subst; subst2; subst1], subst_to_hm_type beta subst)
			| None -> raise Not_typed)
	| HM_Let (x, lmb1, lmb2) -> let (subst1, term1) = infer_hm_type lmb1 context in
		let new_context = String_map.map (fun term -> subst_to_hm_type term subst1) context in
		let (subst2, term2) = infer_hm_type lmb2 (String_map.add x (List.fold_left (fun term var -> if List.mem var (String_map.fold (fun a b c -> (free_vars b [] []) @ c) new_context []) then 
			term
		else
			HM_ForAll (var, term)) term1 (free_vars term1 [] [])) new_context) in
		(compose [subst2; subst1], term2);;

let algorithm_w alpha = 
	counter := 0;
	let beta = rename alpha String_map.empty in
	(try Some (infer_hm_type beta (get_context beta String_map.empty [])) with Not_typed -> None);;