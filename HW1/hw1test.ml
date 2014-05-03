let my_subset_test0 = not (subset [2;5] [1;3;4;8;10])
let my_subset_test1 = subset [1;1;1;] [2;2;2;2;1]

let my_proper_subset_test0 = not (proper_subset [5;6] [6;5;5;6])
let my_proper_subset_test1 = proper_subset [1;2;3] [1;2;3;4]

let my_equal_sets_test0 = equal_sets [2;1;3;2] [3;3;1;2]
let my_equal_sets_test1 = not (equal_sets [1;2;3] [4;3;2])

let my_set_diff_test0 = equal_sets (set_diff [1;2;3;4] [5;6;7]) [1;2;3;4] 
let my_set_diff_test1 = equal_sets (set_diff [1;2;3] []) [1;2;3]

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x ** 2.) 2. = infinity

let my_computed_periodic_point_test0 = computed_periodic_point (=) (fun x -> x / 2 + 1) 0 (-1) = -1

type ingredient_nonterminals = 
  | Sandwich | Bread | Meat | Cheese | Sauce

let sandwich_rules = 
  [Sandwich, [N Bread; N Meat; N Cheese; N Sauce];
   Bread, [T"White"];
   Bread, [T"Wheat"];
   Meat, [T"Turkey"];
   Meat, [T"Ham"];
   Cheese, [T"Swiss"];
   Sauce,[T"Barbeque"]]
let sandwich_grammer = Sandwich, sandwich_rules

let my_sandwich_test0 = filter_blind_alleys sandwich_grammer = sandwich_grammer
let my_sandwich_test1 = filter_blind_alleys (Sandwich, [Sandwich, [N Bread; N Meat; N Cheese; N Sauce];
   Bread, [T"White"];
   Bread, [T"Wheat"];
   Meat, [T"Turkey"];
   Meat, [T"Ham"];
   Cheese, [T"Swiss"]]) = (Sandwich, [Bread, [T"White"];
   Bread, [T"Wheat"];
   Meat, [T"Turkey"];
   Meat, [T"Ham"];
   Cheese, [T"Swiss"]])