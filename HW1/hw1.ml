(* Subset: returns true iff a is a subet of b *)
let rec subset a b = match a, b with
	| [], _ -> true
	| _, [] -> false
	| [a], [b] -> if a == b then true else false
	| h1::t1, h2::t2 -> if (if h1 = h2 then true else subset [h1] t2) then subset t1 b else false;;

(* Proper subset: returns true iff a is a proper subset of b.
 * No set is a proper subset of itself *)
let proper_subset a b = subset a b && not (subset b a);;

(* Equal set: returns true iff the sets are equal. *)
let equal_sets a b = subset a b && subset b a;;

(* Set diff: returns a list representing a−b, that is, the set of all members of a that are not also members of b.*)
let rec set_diff a b = match a, b with
	| [], _ -> []
	| _, [] -> a
	| h1::t1, b -> if (subset [h1] b) then set_diff t1 b else h1::(set_diff t1 b);; 

(* Computed fixed point: returns the computed fixed point for f with respect to x, 
 * assuming that eq is the equality predicate for f's domain. *)
let rec computed_fixed_point eq f x = if (eq (f x) x) 
	then x 
	else (computed_fixed_point eq f (f x));; 

(* Periodic point: returns the computed periodic point for f with period p and with respect to x, 
 * assuming that eq is the equality predicate for f's domain. *)
let rec computed_periodic_point eq f p x = match p with
	| 0 -> x
	| _ -> if eq x (f (computed_periodic_point eq f (p-1) (f x))) 
		then x 
		else (computed_periodic_point eq f p (f x));;

(* Filter blind alleys: returns a copy of the grammar g with all blind-alley rules removed. 
 * This function should preserve the order of rules: that is, all rules that are returned should 
 * be in the same order as the rules in g. *)

(* Define symbol type *)
type ('terminal, 'nonterminal) symbol = 
	| T of 'terminal 
	| N of 'nonterminal;;

(* Check if subrule is good *)
let is_subrule_good good_rules = function
	| T s -> true
	| N s -> subset [s] good_rules;;

(* Rule: a b pair where a is a non-terminal symbol and b is list of subrules. *)
let rec is_rule_good good_rules = function
	| [] -> true
	(* Check that each subrule is terminal *)
	| h::t -> if (is_subrule_good good_rules h) then is_rule_good good_rules t else false;;

(* Find the set of terminal (good) symbols. *)
let rec core_terminal_set good_rules = function
	| [] -> good_rules
	| (a, b)::t -> if (is_rule_good good_rules b)
		then (if (subset [a] good_rules) then core_terminal_set good_rules t else core_terminal_set (a::good_rules) t)
		else core_terminal_set good_rules t;;

(* Helper function to return the correct function type for computed fixed point. *)
let fixed_point_core_set (good_rules, rules) =
	((core_terminal_set good_rules rules), rules);;

let compute_good_rules (good_rules, rules) =  
	fst(computed_fixed_point (fun (a, _) (b, _) -> equal_sets a b) fixed_point_core_set ([], rules));;

(* If rule is good, include in return, else ignore. *)
let rec check_rules good_rules = function
	| [] -> []
	| (a, b)::t -> if (is_rule_good good_rules b) 
		then (a, b)::(check_rules good_rules t) 
		else check_rules good_rules t;;	

(* Use computed fixed point to find the complete set of good rules. *)
let filter_blind_alleys  = function
	| (start_symbol, rules) -> (start_symbol, check_rules (compute_good_rules ([], rules)) rules);; 

