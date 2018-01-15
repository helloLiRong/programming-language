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

(* problem 1 *)
fun only_capitals(strs) =
  List.filter (fn s => Char.isUpper (String.sub(s,0))) strs

(* problem 2 *)	      
fun longest_string1(strs) =
  foldl (fn (x, longest_string) => if String.size x > String.size longest_string
				   then x
				   else longest_string) "" strs

(* problem 3 *)
fun longest_string2(strs) =
  foldl (fn (x, longest_string) => if String.size x >= String.size longest_string
				   then x
				   else longest_string) "" strs
(* problem 4 *)
fun longest_string_helper f strs =
  foldl (fn (x, longest_string) => if f (x, longest_string) then x else longest_string) "" strs

val longest_string3 =
    longest_string_helper (fn (x, longest_string) => String.size x > String.size longest_string)

val longest_string4 =
    longest_string_helper (fn (x, longest_string) => String.size x >= String.size longest_string)

(* problem 5 *)
val longest_capitalized =
    longest_string1 o only_capitals

(* problem 6 *)
val rev_string =
    implode o rev o explode

(* problem 7 *)
fun first_answer f list =
  case list of
      [] => raise NoAnswer
    | x::xs' => case f x of
		    NONE => first_answer f xs'
		  | SOME v => v

(* problem 8 *)
fun all_answers f list =
 let fun helper (xs, acc) =
       case xs of
	   [] => acc
	 | x::xs => case f x of
			NONE => NONE
		      | SOME v => helper (xs, SOME(v @ valOf(acc)))
 in
     helper(list, SOME [])
 end	       		     
      
(* problem 9a *)
val count_wildcards =
  g (fn _=> 1) (fn x:string => 0)

(* problem 9b *)
val count_wild_and_variable_lengths =
    g (fn _ => 1) (fn x => String.size x)

(* problem 9c *)      
fun count_some_var (strs, p) =
  g (fn _ => 0) (fn x => if x = strs then 1 else 0) p
  
(* problem 10 *)
fun check_pat p =
  let fun find_var pat =
	case pat of
	    Variable x => [x]
	   |TupleP ps => List.foldl (fn (p', acc) => (find_var p') @ acc) [] ps
	   |ConstructorP (s', p') => find_var p'
	   | _ => []		      
      fun check_exists strs =
	case strs of
	    [] => true
	  | x::xs => (not (List.exists (fn x' => x' = x) xs)) andalso check_exists xs									
  in
      check_exists (find_var p)
  end
      
		      
(* problem 11 *)
fun match (value, pat) =
  case (value, pat) of
      (_, Wildcard) => SOME []
    | (v, Variable s) => SOME [(s,v)]
    | (Unit, UnitP) => SOME []
    | (Const i, ConstP j) => if i = j then SOME [] else NONE
    | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
			       then all_answers (fn x => match (#1 x,#2 x)) (ListPair.zip(vs,ps))
			       else NONE					
    | (Constructor(s2,v), ConstructorP(s1,p)) => if s1 = s2
						 then match(v,p)
						 else NONE							  
    | _ => NONE
	       

(* problem 12 *)
fun first_match (value, ps) =
 SOME (first_answer (fn x => match (value, x)) ps)
	       handle NoAnswer => NONE
    
(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
