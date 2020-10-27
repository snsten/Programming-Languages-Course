(* Coursera Programming Languages, Homework 3, Provided Code *)


(**** you can put all your code here ****)

(* 1 *)
fun only_capitals sl = List.filter (fn x => Char.isUpper(String.sub(x, 0))) sl

(* 2 *)
fun longest_string1 sl = List.foldl (fn (x, y)=> if String.size x > String.size y then x else y)  "" sl

(* 3 *)
fun longest_string2 sl = List.foldl (fn (x, y)=> if String.size x >= String.size y then x else y) "" sl

(* 4 *)
fun longest_string_helper f sl = List.foldl (fn (x, y) => if f (String.size x, String.size y) then x else y) "" sl

val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)
					    
(* 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* 6 *)
val rev_string = implode o rev o explode


exception NoAnswer
	      
(* 7 *)
fun first_answer f g =
    case g of
	[] => raise NoAnswer
     |  x::xs => case f x of
		     NONE => first_answer f xs
		  |  SOME v => v

(* 8 *)
fun all_answers f l =
    let
	fun aux (acc, l) =
	    case l of
		[] => SOME acc
	       | x::xs => case f x of
			      NONE => NONE
			    | SOME v => aux (v @ acc, xs)
    in
	aux ([], l)
    end

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


(* 9 *)
val count_wildcards = g (fn () => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn () => 1) String.size

fun count_some_var (s, p) = g (fn () => 0) (fn x => if x=s then 1 else 0) p

(* 10 *)
fun check_pat p =
    let
	fun pattern p' =
	    case p' of
		Variable x => [x]
	      | TupleP ps => List.foldl (fn (x, y) => y @ (pattern x)) [] ps
	      | ConstructorP(_, p) => pattern p
	      | _ => []
			 
	fun exists i=
	    case i of
		[] => false
	      | x::xs => List.exists (fn z => x=z) xs
    in
	not ((exists o pattern) p)
    end

(* 11 *)
fun match (vl, pt) =
    case (vl, pt) of
	(_, Wildcard) => SOME []
      | (v, Variable x) => SOME [(x, v)]
      | (Unit, UnitP) => SOME []
      | (Const c, ConstP x) => if c=x
			       then SOME []
			       else NONE
      | (Tuple t1, TupleP t2) => if List.length t1 = List.length t2
				 then all_answers match (ListPair.zip (t1, t2))
				 else NONE
      | (Constructor(x, c1), ConstructorP(y, c2)) => if x=y
						     then match (c1, c2)
						     else NONE
      | (_, _) => NONE

(* 12 *)
fun first_match vl pt =
    SOME (first_answer (fn x => match(vl, x)) pt)
    handle NoAnswer => NONE
	 
												
(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

