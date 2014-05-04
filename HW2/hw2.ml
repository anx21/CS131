type ('terminal, 'nonterminal) symbol = 
	| T of 'terminal 
	| N of 'nonterminal;;

(* Write a function convert_grammar gram1 that returns a Homework 2-style grammar, 
 which is converted from the Homework 1-style grammar gram1. Test your implementation 
 of convert_grammar on the test grammars given in Homework 1. For example, the top-level
 definition let awksub_grammar_2 = convert_grammar awksub_grammar should bind awksub_grammar_2 
 to a Homework 2-style grammar that is equivalent to the Homework 1-style grammar awksub_grammar. *)

(* Type: 'a -> ('a * 'b') list -> 'b list *)
let rec rule_list nt rules = match rules with
	| [] -> []
	| (lhs, rhs)::t -> if (lhs = nt)
		then rhs::(rule_list nt t)
		else rule_list nt t;;

(* Type: 'a * ('b * 'c) list -> 'a * ('b * 'c) list *)
let convert_grammar gram1 = match gram1 with
	| start_symbol, rules -> (start_symbol, fun nt -> (rule_list nt rules));;

(* Write a function parse_prefix gram that returns a matcher for the grammar gram. 
 When applied to an acceptor accept and a fragment frag, the matcher must return the 
 first acceptable match of a prefix of frag, by trying the grammar rules in order; 
 this is not necessarily the shortest nor the longest acceptable match. A match is 
 considered to be acceptable if accept succeeds when given a derivation and the suffix 
 fragment that immediately follows the matching prefix. When this happens, the matcher 
 returns whatever the acceptor returned. If no acceptable match is found, the matcher returns None. *)

(* Similar to match_some_symbols 
 * Type: ('a -> ('b, 'a) symbol list list) ->
  ('b, 'a) symbol list ->
  (('a * ('b, 'a) symbol list) list -> 'b list -> 'c option) ->
  ('a * ('b, 'a) symbol list) list -> 'b list -> 'c option = <fun> *)
let rec match_and rules rule accept deriv frag = match rule with
	(* Rule has been matched -> call acceptor with derivation and fragment (unmatched suffix) *)
	| [] -> accept deriv frag 
	| _ -> match frag with
		(* Pick a symbol to match *)
		(* Fragment is exhauted but rule is not -> None *)
		| [] -> None
		| h::t -> match rule with 
			| (T t_sym)::t2 -> if h = t_sym 
				(* Match rest of rule and fragment *)
				then (match_and rules t2 accept deriv t)
				else None
			(* On nontermainl symbol, start another recursively stack but with a new acceptor that
			 matches the suffix  and tail of the current rule *)
			| (N nt_sym)::t2 -> (match_or nt_sym rules (rules nt_sym) (match_and rules t2 accept) deriv frag)

(* Simultaneous definition, similar to match_a_nonterminal 
 * Type: 'a ->
  ('a -> ('b, 'a) symbol list list) ->
  ('b, 'a) symbol list list ->
  (('a * ('b, 'a) symbol list) list -> 'b list -> 'c option) ->
  ('a * ('b, 'a) symbol list) list -> 'b list -> 'c option = <fun> *)
and match_or start_symbol rules rules_matching_start_symbol accept deriv frag = 
	match rules_matching_start_symbol with
		(* Exhausted rules -> None *)
		| [] -> None
		(* Try first rule *)
		| fst_rule::rest_of_rules -> match (match_and rules fst_rule accept (deriv@[start_symbol, fst_rule]) frag) with
			(* Move onto next rule *)
			| None -> (match_or start_symbol rules rest_of_rules accept deriv frag)
			(* Return what match_and returns *)
 			| x -> x

(* Type: 'a * ('a -> ('b, 'a) symbol list list) ->
  (('a * ('b, 'a) symbol list) list -> 'b list -> 'c option) ->
  'b list -> 'c option = <fun> *)
let parse_prefix grammar accept frag = match grammar with
	| (start_symbol, rules) -> match_or start_symbol rules (rules start_symbol) accept [] frag;;
