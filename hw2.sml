(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (str, strList) =
    let
	fun compare_lists (currList, acc) =
	    case currList of
		[] => NONE
	      | x :: xs => if same_string (str, x)
			   then SOME (acc @ xs)
			   else compare_lists (xs, x::acc)
    in
	compare_lists (strList, [])
    end

fun get_substitutions1 (strList, str) =
    case strList of
	[] => []
      | x :: xs => case all_except_option (str, x) of
		       NONE => get_substitutions1 (xs, str)
		     | SOME xs' => xs' @ get_substitutions1 (xs, str)


fun get_substitutions2 (strList, str) =
    let
	fun substitute (strList, acc) =
	    case strList of
		[] => acc
	      | x :: xs => case all_except_option (str, x) of
			       NONE => substitute (xs, acc)
			     | SOME xs' => substitute (xs, xs' @ acc)
    in
	substitute (strList, [])
    end

fun similar_names (subsList, name) =
    let
	fun substitute (subs, name) =
	    case subs of
		[] => []
	      | sub :: xs => case name of {first=x, last=y, middle=z} =>
					  [{first=sub, last=y, middle=z}] @ substitute (xs, name)
    in
	case name of
	    {first=x, middle=y, last=z} =>
	    let
		val fname = get_substitutions2 (subsList, x)
	    in
		name :: substitute (fname, name)
	    end
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

fun card_color (suit, rank) =
    case suit of
	Spades => Black
      | Clubs => Black
      | _ => Red 

fun card_value (suit, rank) =
    case rank of
	Ace => 11
      | Jack => 10
      | King => 10
      | Queen => 10
      | Num n => n

		     
fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
      | card :: cs' =>
	if card = c
	then cs'
	else card :: remove_card (cs', c, e)

fun all_same_color (cs) =
    case cs of
	[] => true
      | card :: [] => true
      | first :: second :: cs' =>
	card_color (first) = card_color (second) andalso
	all_same_color (second :: cs')

fun sum_cards (cs) =
    let
	fun tail_sum (cs, acc) =
	    case cs of
		[] => acc
	      | card :: cs' => tail_sum(cs', acc + card_value (card))
    in
	tail_sum(cs, 0)
    end
	
fun score (cs, goal) =
    let
	val sum = sum_cards (cs)
	val prelim_score = if sum > goal
			   then 3 * (sum - goal)
			   else goal - sum
    in
	if all_same_color (cs)
	then prelim_score div 2
	else prelim_score
    end
	
	
fun officiate (cs, moves, goal) =
    let
	fun next_state (held, moves, cards) =
	    if sum_cards (held) > goal
	    then score (held, goal)
	    else case moves of
		     [] => score (held, goal)
		   | x :: xs => case x of
				    Discard card =>
				    next_state (remove_card (held, card, IllegalMove), xs, cards)
				  | Draw => case cards of
						[] => score (held, goal)
					      | y :: ys => next_state (y :: held, xs, ys)
    in
	next_state ([], moves, cs)
    end
	
