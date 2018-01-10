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
fun all_answer f list =
  let fun helper (xs, acc) =
	case xs of
	    [] => acc
	  | x::xs => case f x of
			 NONE => helper(xs, acc)
		       | SOME lst => helper(xs,acc @ lst)
  in
      
  end
      
	      
(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)



(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["Aa","bc","C"] = "Aa"
						 
val test3 = longest_string2 ["Aa","bc","C"] = "bc"
						 						 
val test4a = longest_string3 ["Aa","bc","C"] = "Aa"

val test4b = longest_string4 ["A","B","C"] = "C"
						 
val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"
				   
val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
										    
(*
val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []
*)
			       
