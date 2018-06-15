(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)


(* question1: get rid of target element, write as tail recursion *)
fun all_except_option(target, pool) =
	let fun helper (res, curr) = 
		case res of [] => curr
			| x::xs' => if same_string(target, x) then helper(xs', curr) else helper(xs', curr @ [x])
		val tmp = helper(pool, [])
	in
		case (pool, tmp) of 
			([], []) => NONE
			| (_, []) => SOME []
			| (_, _)=> if tmp = pool then NONE else SOME(tmp)
	end


(* question2 non tail recursion of picking all the element in the array with the target element
	but it should not contain the original element *)
fun get_substitutions1(pool, target) = 
	case pool of [] => []
		| x::xs' => case all_except_option(target, x) of NONE => get_substitutions1(xs', target)
									| SOME l => l @ get_substitutions1(xs', target)


(* problem3 tail recursion version of question 2 *)
fun get_substitutions2(pool, target) =
	let fun helper(remain, curr) = 
		case remain of [] => curr
			| x::xs' => case all_except_option(target, x) of NONE => helper(xs', curr)
									| SOME l => helper(xs', curr @ l)
	in
		helper(pool, [])
	end


(* problem4 tail recursion version *)
fun similar_names(pool, {first, last, middle}) = 
	let 
			fun helper (firstPool, curr) = 
				case firstPool of [] => curr
					| x::xs' => helper (xs', curr @ [{first = x, last = last, middle = middle}])
	in 
		helper(first :: get_substitutions2(pool, first), [])
	end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* a. return the color of the card *)
fun card_color (suit, rank) = 
	case suit of Spades => Black
		| Clubs => Black
		| _ => Red


(* b. return the value of the card *)
fun card_value (suit, rank) = 
	case rank of Ace => 11
		| Num num => num
		| _ => 10


(* c. remove a certain card, if the card appears multiple times, only remove the first one *)
fun remove_card (cs, c, e) = 
	let fun helper (remain, curr) = 
		case remain of [] => curr
			| x::xs' => if c = x then curr @ xs' else helper(xs', curr @ [x])
		val tmp = helper(cs, [])
	in
		if tmp = cs then raise e else tmp
	end


(* d. evaluate if all the card in the pool are same color *)
fun all_same_color (pool) = 
	case pool of [] => true
		| x::[] => true
		| x::x1::[] => card_color(x) = card_color(x1)
		| x::x1::x2 => card_color(x) = card_color(x1) andalso all_same_color(x1::x2)


(* e. sum the value of all the card in the pool, it must use constant stack space *)
fun sum_cards (pool) = 
	let fun helper(remain, curr) = 
		case remain of [] => curr
			| x::xs' => helper(xs', curr + card_value(x))
	in
		helper(pool, 0)
	end


(* f. calculate score with given pool and goal *)
fun score (pool, goal) = 
	let 
		val raw = sum_cards(pool) - goal
		val preliminary = not (all_same_color(pool))
	in
		if raw > 0 then 
			if preliminary then 3 * raw
			else 3 * raw div 2
		else
			if preliminary then ~raw
			else (~raw) div 2
	end


(* g. run the game *)
fun officiate (card_list, move_list, goal) = 
	let fun helper (card_held, card_remain, move_list) = 
		case (card_remain, move_list, sum_cards(card_held) > goal) of
			(* score bomb *)
			(_, _, true) => score(card_held, goal)
			(* no more moves *)
			| (_, [], false) => score(card_held, goal)
			(* remove card *)
			| (_, (Discard card) :: moves', false) => helper(remove_card(card_held, card, IllegalMove), card_remain, moves')
			(* draw card, depends on card stack, if the stack is empty *)
			| ([], Draw::moves', false) => score(card_held, goal)
			(* draw card, depends on card stack, if the stack is not empty *)
			| (card::cards', Draw::moves', false) => helper(card::card_held, cards', moves')
	in
		helper([], card_list, move_list)
	end


(* helper function, replace one 11 ace with one 1 ace *)
fun replace_one_ace (card_list) = 
	let fun helper (remain, curr) = 
		case remain of [] => curr
		| (suit, Ace)::xs' => curr @ [(suit, Num 1)] @ xs'
		| x::xs' => helper(xs', curr @ [x])
	in
		helper(card_list, [])
	end


(* challenge problem 1 *)
fun score_challenge (pool, goal) = 
	let
		val curr_score = score(pool, goal)
		val new_pool = replace_one_ace(pool)
	in
		if new_pool = pool then curr_score
		else
			if score(new_pool, goal) < curr_score then score_challenge(new_pool, goal)
			else curr_score
	end 


(* helper function for officiate_challenge, parrallel with replace one ace 
	replace one "Remove act action to Romove Num 1 action" *)
fun replace_one_removeAce (move_list) = 
	let fun helper (remain, curr) = 
		case remain of [] => curr
		| (Discard (suit, Ace))::xs' => curr @ [(Discard (suit, Num 1))] @ xs'
		| x::xs' => helper(xs', curr @ [x])
	in
		helper(move_list, [])
	end


(* challenge problem 2 *)
fun officiate_challenge (card_list, move_list, goal) = 
	let
		val curr_score = officiate (card_list, move_list, goal)
		val new_card_list = replace_one_ace(card_list)
		val new_move_list = replace_one_removeAce(move_list)
		val new_score = officiate (new_card_list, new_move_list, goal)
	in
		if new_card_list = card_list then curr_score
		else
			if new_score < curr_score 
			then officiate_challenge(new_card_list, new_move_list, goal) 
			else curr_score
	end 

(* challenge problem 3 *)
fun careful_player(card_list, goal) = 
	let
		fun explore(card, card_held, curr_total) = 
			case card_held of 
				[] => []
				| x::xs' => if curr_total - card_value(x) + card_value(card) = goal then [Discard x, Draw]
										else explore(card, xs', curr_total)

		fun strategy (card_stack, card_held, move_list) = 
			case (card_stack, score_challenge(card_held, goal) = 0) of
				(_, true) => move_list
				| ([], _) => move_list
				| (card::cards', false) => 
				if sum_cards(card_held) < goal - 10 
				then strategy(cards', card_held @ [card], move_list @ [Draw])
				else if sum_cards(card::card_held) > goal then
					case explore(card, card_held, sum_cards(card_held)) of 
						[] => move_list
						|	x::xs' => move_list @ [x] @ xs'	
					else strategy(cards', card_held @ [card], move_list @ [Draw])
	in
		strategy (card_list, [], [])
	end

