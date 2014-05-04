type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

let awksub_rules = 
	[Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]
let awksub_grammar = Expr, awksub_rules

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
     [[N Num];
      [N Lvalue];
      [N Incrop; N Lvalue];
      [N Lvalue; N Incrop];
      [T"("; N Expr; T")"]]
     | Lvalue ->
     [[T"$"; N Expr]]
     | Incrop ->
     [[T"++"];
      [T"--"]]
     | Binop ->
     [[T"+"];
      [T"-"]]
     | Num ->
     [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
      [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

let awkish_grammar_simple = (Num,
  function | Num -> [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"]; [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

let awksub_grammar_2 = convert_grammar awksub_grammar
let my_convert_grammar_test_0 = snd(awksub_grammar_2) Num = snd(awkish_grammar) Num
let my_convert_grammar_test_1 = snd(awksub_grammar_2) Binop = snd(awkish_grammar) Binop

let accept_all derivation string = Some (derivation, string)
let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None

let test_0 = (parse_prefix awkish_grammar_simple accept_all ["0"]) = Some ([(Num, [T "0"])], [])

let test0 = ((parse_prefix awkish_grammar accept_all ["ouch"]) = None)
let test1 =
  ((parse_prefix awkish_grammar accept_all ["9"])
   = Some ([(Expr, [N Term]); (Term, [N Num]); (Num, [T "9"])], []))
let test2 =
  ((parse_prefix awkish_grammar accept_all ["9"; "+"; "$"; "1"; "+"])
   = Some
       ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "9"]);
   (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Lvalue]);
   (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Num]);
   (Num, [T "1"])],
  ["+"]))
let test3 =
  ((parse_prefix awkish_grammar accept_empty_suffix ["9"; "+"; "$"; "1"; "+"])
   = None)

  (* This one might take a bit longer.... *)
let test4 =
 ((parse_prefix awkish_grammar accept_all
     ["("; "$"; "8"; ")"; "-"; "$"; "++"; "$"; "--"; "$"; "9"; "+";
      "("; "$"; "++"; "$"; "2"; "+"; "("; "8"; ")"; "-"; "9"; ")";
      "-"; "("; "$"; "$"; "$"; "$"; "$"; "++"; "$"; "$"; "5"; "++";
      "++"; "--"; ")"; "-"; "++"; "$"; "$"; "("; "$"; "8"; "++"; ")";
      "++"; "+"; "0"])
  = Some
     ([(Expr, [N Term; N Binop; N Expr]); (Term, [T "("; N Expr; T ")"]);
       (Expr, [N Term]); (Term, [N Lvalue]); (Lvalue, [T "$"; N Expr]);
       (Expr, [N Term]); (Term, [N Num]); (Num, [T "8"]); (Binop, [T "-"]);
       (Expr, [N Term; N Binop; N Expr]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Incrop; N Lvalue]); (Incrop, [T "++"]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Incrop; N Lvalue]); (Incrop, [T "--"]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Num]); (Num, [T "9"]); (Binop, [T "+"]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term; N Binop; N Expr]);
       (Term, [N Lvalue]); (Lvalue, [T "$"; N Expr]);
       (Expr, [N Term; N Binop; N Expr]); (Term, [N Incrop; N Lvalue]);
       (Incrop, [T "++"]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Num]); (Num, [T "2"]); (Binop, [T "+"]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term]); (Term, [N Num]);
       (Num, [T "8"]); (Binop, [T "-"]); (Expr, [N Term]); (Term, [N Num]);
       (Num, [T "9"]); (Binop, [T "-"]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue; N Incrop]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue; N Incrop]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Incrop; N Lvalue]);
       (Incrop, [T "++"]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Lvalue; N Incrop]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Num]); (Num, [T "5"]); (Incrop, [T "++"]); (Incrop, [T "++"]);
       (Incrop, [T "--"]); (Binop, [T "-"]); (Expr, [N Term]);
       (Term, [N Incrop; N Lvalue]); (Incrop, [T "++"]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]); (Term, [N Lvalue; N Incrop]);
       (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [T "("; N Expr; T ")"]); (Expr, [N Term]);
       (Term, [N Lvalue; N Incrop]); (Lvalue, [T "$"; N Expr]); (Expr, [N Term]);
       (Term, [N Num]); (Num, [T "8"]); (Incrop, [T "++"]); (Incrop, [T "++"]);
       (Binop, [T "+"]); (Expr, [N Term]); (Term, [N Num]); (Num, [T "0"])],
      []))

let rec contains_lvalue = function
  | [] -> false
  | (Lvalue,_)::_ -> true
  | _::rules -> contains_lvalue rules

let accept_only_non_lvalues rules frag =
  if contains_lvalue rules
  then None
  else Some (rules, frag)

let test5 =
  ((parse_prefix awkish_grammar accept_only_non_lvalues
      ["3"; "-"; "4"; "+"; "$"; "5"; "-"; "6"])
   = Some
      ([(Expr, [N Term; N Binop; N Expr]); (Term, [N Num]); (Num, [T "3"]);
  (Binop, [T "-"]); (Expr, [N Term]); (Term, [N Num]); (Num, [T "4"])],
       ["+"; "$"; "5"; "-"; "6"]))

type english_nonterminals =
  | Sentence | NP | VP | Noun | Verb | Deter | PP | P

let simple_english_grammar = (Sentence,
  function | Sentence -> [[N NP; N VP]]
    | NP -> [[N Deter; N Noun]; [N Noun]]
    | VP -> [[N Verb; N NP]; [N Verb]; [N Verb; N NP; N PP]; [N Verb; N PP]]
    | Noun -> [[T"i"]; [T"he"]; [T"girl"]; [T"soccer"]; [T"ball"]; [T"hotdog"]; [T"pizza"]; [T"park"]; [T"class"]]
    | Verb -> [[T"play"]; [T"eat"]; [T"kick"]]
    | Deter -> [[T"the"]; [T"a"]]
    | PP -> [[N P; N NP]]
    | P -> [[T"at"]; [T"in"]])

let test_1 = ((parse_prefix simple_english_grammar accept_empty_suffix ["i"; "eat"; "in"; "class"])
  = Some
 ([(Sentence, [N NP; N VP]); (NP, [N Noun]); (Noun, [T "i"]);
   (VP, [N Verb; N PP]); (Verb, [T "eat"]); (PP, [N P; N NP]); (P, [T "in"]);
   (NP, [N Noun]); (Noun, [T "class"])], []))

let test_2 = ((parse_prefix simple_english_grammar accept_empty_suffix ["he"; "play"; "soccer"; "at"; "the"; "park"]) 
 = Some
 ([(Sentence, [N NP; N VP]); (NP, [N Noun]); (Noun, [T "he"]);
   (VP, [N Verb; N NP; N PP]); (Verb, [T "play"]); (NP, [N Noun]);
   (Noun, [T "soccer"]); (PP, [N P; N NP]); (P, [T "at"]);
   (NP, [N Deter; N Noun]); (Deter, [T "the"]); (Noun, [T "park"])], []))

let test_3 = ((parse_prefix simple_english_grammar accept_empty_suffix ["the"; "girl"; "eat"; "the"; "pizza"; "at"; "the"; "park"]
 =  ([(Sentence, [N NP; N VP]); (NP, [N Deter; N Noun]); (Deter, [T "the"]);
   (Noun, [T "girl"]); (VP, [N Verb; N NP; N PP]); (Verb, [T "eat"]);
   (NP, [N Deter; N Noun]); (Deter, [T "the"]); (Noun, [T "pizza"]);
   (PP, [N P; N NP]); (P, [T "at"]); (NP, [N Deter; N Noun]);
   (Deter, [T "the"]); (Noun, [T "park"])], [])))