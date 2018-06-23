(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* problem 1, filter all the string start with Capital *)
fun only_capitals pool = 
	List.filter (fn s => Char.isUpper(String.sub(s, 0))) pool

(* problem 2, find the longest string in the list*)
fun longest_string1 pool = 
	List.foldl (fn (s1,s2) => if String.size(s1) > String.size(s2) then s1 else s2) "" pool

(* problem 3, same to problem 2, if there is a tie, return the one near the end of the list *)
fun longest_string2 pool = 
	List.foldl (fn (s1,s2) => if String.size(s2) > String.size(s1) then s2 else s1) "" pool

(* helper function of problem 4 *)
fun longest_string_helper f pool = 
	List.foldl (fn (s1, s2) => if f(String.size(s1), String.size(s2)) then s1 else s2) "" pool

(* the functionality is same as problem 2 *)
fun longest_string3 pool = 
	longest_string_helper (fn (a,b) => if a > b then true else false) pool

(* the functionality is same as problem 3 *)
fun longest_string4 pool = 
	longest_string_helper (fn (a,b) => if b > a then false else true) pool

(* problem 5, find the longest string with capitalize first letter *)
fun longest_capitalized pool = 
	(longest_string1 o only_capitals) pool

(* problem 6, reverse a string *)
fun rev_string s = 
	(String.implode o List.rev o String.explode) s

(* problem 7, find the first answer has the form SOME *)
fun first_answer f pool = 
	case pool of
		[] => raise NoAnswer
		| x :: xs' => case f x of SOME y => y
			| _ => first_answer f xs'

(* problem 8, find all the answer if all have answer, else NONE *)
fun all_answers f pool = 
	let fun helper (func, xs, acc) = 
		case xs of
			[] => SOME(acc)
			| x :: xs' => case func x of
				SOME y => helper (func, xs', acc @ y)
				| NONE => NONE
	in
		helper(f, pool, [])
	end

(* problem 9a, count how many wildcard matching *)
fun count_wildcards p = 
	g (fn () => 1) (fn x => 0) p

(* problem 9b, wild card and variable string length *)
fun count_wild_and_variable_lengths p = 
	g (fn () => 1) (fn x => String.size(x)) p

(* problem 9c, count a certain string match Variable times *)
fun count_some_var (s, p) = 
	g (fn () => 0) (fn x => if x = s then 1 else 0) p

(* problem 10, chekc if all the string elements are distinct *)
fun check_pat p = 
	let
		fun all_pattern patt = 
			case patt of Variable x => [x]
				| TupleP ps => List.foldl (fn (tmp, acc) => acc @ all_pattern tmp) [] ps
				| ConstructorP (s, pt) => all_pattern pt
				| _ => []
		fun not_duplicate (pool) = 
			case pool of [] => true
				| x :: xs' => if List.exists (fn s => s = x) xs' then false else not_duplicate (xs')
	in
		not_duplicate(all_pattern p)
	end


(* problem 11, match value and pattern *)
fun match (value, pattern) = 
	case (value, pattern) of
		(_, Wildcard) => SOME []
		| (v , Variable s) => SOME [(s, v)]
		| (Unit, UnitP) => SOME []
		| (Const c, ConstP cp) => if c = cp then SOME[] else NONE
		| (Constructor (cs, cv), ConstructorP (cps, cp)) => if cs = cps then match(cv, cp) else NONE
		| (Tuple t, TupleP tp) => if List.length(t) = List.length(tp) then all_answers match (ListPair.zip(t, tp)) else NONE
		| (_, _) => NONE


(* problem 12, find the first match *)
fun first_match v ps = 
	SOME(first_answer (fn x => match(v, x)) ps)
	handle NoAnswer => NONE



