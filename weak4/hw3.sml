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
val only_capitals =
  List.filter (fn s => Char.isUpper (String.sub(s,0)))

(* problem 2 *)	      
val longest_string1 =
  foldl (fn (x, longest_string) => if String.size x > String.size longest_string
				   then x
				   else longest_string) ""

(* problem 3 *)
val longest_string2 =
  foldl (fn (x, longest_string) => if String.size x >= String.size longest_string
				   then x
				   else longest_string) ""
(* problem 4 *)
fun longest_string_helper f =
  foldl (fn (x, longest_string) => if f (String.size x, String.size longest_string) then x else longest_string) ""

val longest_string3 =
    longest_string_helper (fn (size_of_x, size_of_longest_string) => size_of_x > size_of_longest_string)

val longest_string4 =
    longest_string_helper (fn (size_of_x, size_of_longest_string) => size_of_x >= size_of_longest_string)

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
	   [] => SOME acc
	 | x::xs => case f x of
			NONE => NONE
		      | SOME v => helper(xs, acc @ v)
 in
     helper(list, [])
 end	       		     
      
(* problem 9a *)
val count_wildcards =
  g (fn _=> 1) (fn x => 0)

(* problem 9b *)
val count_wild_and_variable_lengths =
    g (fn _ => 1) String.size

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
fun first_match value ps =
 SOME (first_answer (fn x => match (value, x)) ps)
	       handle NoAnswer => NONE
    
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
val test101 = only_capitals ["a","B","C"] = ["B","C"]
val test102 = only_capitals ["Abc","ABc","abC"] = ["Abc","ABc"]
val test103 = only_capitals ["1AB","?AB","Abc","ABc","abC"] = ["Abc","ABc"]

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test201 = longest_string1 ["A","bc","C", "de"] = "bc"
val test202 = longest_string1 ["A","bc","C", "def"] = "def"

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test301 = longest_string2 ["A","bc","C", "de"] = "de"
val test302 = longest_string2 ["A","bc","C", "def"] = "def"

val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4a1 = longest_string3 ["A","bc","C", "de"] = "bc"
val test4a2 = longest_string3 ["A","bc","C", "def"] = "def"

val test4b = longest_string4 ["A","B","C"] = "C"
val test4b1 = longest_string4 ["A","bc","C", "de"] = "de"
val test4b2 = longest_string4 ["A","bc","C", "def"] = "def"

val test5 = longest_capitalized ["A","bc","C"] = "A"
val test501 = longest_capitalized [] = ""
val test502 = longest_capitalized ["ab", "a", "b"] = ""

val test6 = rev_string "abc" = "cba"
val test601 = rev_string "" = ""

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test701 = first_answer (fn x => if x > 3 then SOME x else NONE) [4,2,3,5] = 4
val test702 = (first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3] ; false) handle NoAnswer => true
val test7022 = (first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3] ; false) handle OtherException => true
val test703 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,2] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test801 = all_answers (fn x => if x = 2 then SOME [x] else NONE) [3,2,4,5,6,7] = NONE
val test802 = all_answers (fn x => if x mod 2 = 0 then SOME [x] else NONE) [2,4,5,6,8] = NONE
val test803 = all_answers (fn x => if x mod 2 = 0 then SOME [x] else NONE) [2,4,6,8] = SOME [2,4,6,8]
val test804 = all_answers (fn x => if x mod 2 = 0 then SOME [x, x + 1] else NONE) [2,4,6,8] = SOME [2,3,4,5,6,7,8,9]
val test805 = all_answers (fn x => if x mod 2 = 0 then SOME [] else NONE) [2,4,6,8] = SOME []
val test806 = all_answers (fn x => if x mod 2 = 0 then SOME [x] else NONE) [] = SOME []

val test9a = count_wildcards Wildcard = 1
val test9a01 = count_wildcards (Variable "str") = 0
val test9a02 = count_wildcards (TupleP [Wildcard, ConstP 12, Wildcard]) = 2
val test9a03 = count_wildcards (ConstructorP("pattern", (TupleP [Wildcard, ConstP 12, Wildcard]))) = 2

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b01 = count_wild_and_variable_lengths Wildcard = 1
val test9b02 = count_wild_and_variable_lengths (TupleP [Wildcard, ConstP 12, Wildcard]) = 2
val test9b03 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "str", Wildcard]) = 5
val test9b04 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "str", Wildcard, Variable "str2"]) = 9
val test9b05 = count_wild_and_variable_lengths (ConstructorP("pattern", (TupleP [Wildcard, ConstP 12, Wildcard]))) = 2
val test9b06 = count_wild_and_variable_lengths (ConstructorP("pattern", (TupleP [Wildcard, Variable "str", Wildcard]))) = 5

val test9c = count_some_var ("x", Variable("x")) = 1
val test9c01 = count_some_var ("x", (TupleP [Wildcard, ConstP 12, Wildcard])) = 0
val test9c02 = count_some_var ("x", (TupleP [Wildcard, Variable "str", Wildcard])) = 0
val test9c03 = count_some_var ("x", (TupleP [Wildcard, Variable "x", Wildcard])) = 1
val test9c04 = count_some_var ("x", (TupleP [Wildcard, Variable "x", Wildcard, Variable "x"])) = 2
val test9c05 = count_some_var ("x", (ConstructorP("pattern", (TupleP [Wildcard, Variable "x", Wildcard])))) = 1
val test9c06 = count_some_var ("x", (ConstructorP("x", (TupleP [Wildcard, Variable "x", Wildcard])))) = 1

val test10 = check_pat (Variable("x")) = true
val test1001 = check_pat (TupleP [Wildcard, Variable "x", Wildcard]) = true
val test1002 = check_pat (TupleP [Wildcard, Variable "x", Variable "y"]) = true
val test1003 = check_pat (TupleP [Wildcard, Variable "x", Variable "x"]) = false
val test1004 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", Wildcard]))) = true
val test1005 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", Variable "y")]))) = true
val test1006 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", Variable "x")]))) = false
val test1007 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", TupleP [Variable "y"])]))) = true
val test1008 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", TupleP [Variable "z"])]))) = true
val test1009 = check_pat (ConstructorP("x", (TupleP [Wildcard, Variable "x", ConstructorP("y", TupleP [Variable "x"])]))) = false
val test1010 = check_pat (ConstructorP("x", (ConstructorP("y", TupleP [Variable "x", Variable "y"])))) = true
val test1011 = check_pat (ConstructorP("x", (ConstructorP("y", TupleP [Variable "x", Variable "x"])))) = false
val test1012 = check_pat (TupleP [Wildcard, Variable "x", TupleP [Variable "y"]]) = true

val test11 = match (Const(1), UnitP) = NONE
val test1101 = match (Const(1), ConstP 1) = SOME []
val test1102 = match (Const(1), Variable "s") = SOME [("s", Const(1))]
val test1103 = match (Const(1), TupleP [Wildcard]) = NONE
val test1104 = match (Const(1), TupleP [ConstP 1]) = NONE
val test1105 = match (Tuple [Unit], TupleP [UnitP]) = SOME []
val test1106 = match (Tuple [Tuple [Unit]], TupleP [TupleP[UnitP]]) = SOME []
val test1107 = match (Tuple [Tuple [Unit]], TupleP [TupleP[UnitP, Variable "x"]]) = NONE
val test1108 = match (Tuple [Const(1), Tuple [Unit]], TupleP [ConstP 1, TupleP[UnitP]]) = SOME []
val test1109 = match (Tuple [Const(1), Tuple [Unit, Const(2)]], TupleP [ConstP 1, TupleP[UnitP, Variable("s")]]) = SOME [("s", Const(2))]
val test1110 = match (Tuple [Const(1), Tuple [Unit, Const(2)]], TupleP [ConstP 2, TupleP[UnitP, Variable("s")]]) = NONE
val test1111 = match (Tuple [Const(1), Tuple [Unit, Const(2)]], TupleP [ConstP 1, TupleP[UnitP, Variable("s"), Wildcard]]) = NONE

val test12 = first_match Unit [UnitP] = SOME []
val test1201 = first_match Unit [Variable ("s")] = SOME [("s", Unit)]
val test1202 = first_match (Tuple [Const(1), Tuple [Unit, Const(2)]]) [(TupleP [ConstP 1, TupleP[UnitP, Variable("s")]])] = SOME [("s", Const(2))]
val test1203 = first_match (Tuple [Const(1), Tuple [Unit, Const(2)]]) [(TupleP [ConstP 1, TupleP[UnitP, ConstP 3]])] = NONE	
			       
