(* Homework 1 *)

fun is_older(date1 : int*int*int, date2 : int*int*int) =
    if (#1 date1) = (#1 date2) then
        if (#2 date1) = (#2 date2) then (#3 date1) < (#3 date2)
        else (#2 date1) < (#2 date2)
    else
        (#1 date1) < (#1 date2)
			
fun number_in_month (dates : (int*int*int) list, month: int) = 
	if null dates then 0
	else
        if (#2 (hd dates) = month) then 1 + number_in_month (tl dates, month)
		else number_in_month (tl dates, month)

fun number_in_months (dates : (int*int*int) list, months : int list) = 
	if null dates orelse null months then 0
	else 
		number_in_month (dates, hd months) + number_in_months (dates, tl months)
	
fun dates_in_month (dates : (int*int*int) list, month: int) = 
	if null dates then []
	else
        if (#2 (hd dates) = month) then hd dates :: dates_in_month (tl dates, month)
		else dates_in_month (tl dates, month)

fun dates_in_months (dates : (int*int*int) list, months : int list) = 
	if null dates orelse null months then []
	else 
		dates_in_month (dates, hd months) @ dates_in_months (dates, tl months)

fun get_nth (strings : string list, n : int) = 
	if null strings then "" else
	    if n = 1 then hd strings else get_nth (tl strings, n - 1)

fun date_to_string (date : (int*int*int)) = 
	let val months = ["January", "February", "March", "April", "May", "June", 
				  "July", "August", "September", "October", "November", "December"]
	in
		get_nth (months, (#2 date)) ^ " " ^ (Int.toString(#3 date)) ^ ", " ^ (Int.toString(#1 date))
	end
	
fun number_before_reaching_sum (sum : int, numbers : int list) : int =
	let fun count (numbers : int list, counter : int, n : int) =
    	if hd numbers + counter >= sum then n 
    	else 
    		count (tl numbers, counter + hd numbers, n + 1)
  	in
    	count (numbers, 0, 0)
  	end
  	
fun what_month (day) : int = 
	let val days_per_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  	in
    	number_before_reaching_sum (day, days_per_month) + 1
  	end

fun month_range (day1, day2) : int list = 
	if day1 > day2 then [] 
	else 
		what_month (day1) :: month_range (day1 + 1, day2)

fun oldest (dates : (int*int*int) list) =
    if null dates then NONE
    else 
        let
            val tl_ans = oldest (tl dates)
        in
            if isSome tl_ans andalso is_older(valOf tl_ans, hd dates) then tl_ans
            else SOME (hd dates)
        end


(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)


val test1 = is_older ((1,2,3),(2,3,4)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
	
	
