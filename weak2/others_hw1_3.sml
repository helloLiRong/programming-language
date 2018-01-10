fun is_older (date1: int*int*int, date2: int*int*int) =
  let
      val dates = [(#1 date1, #1 date2), (#2 date1, #2 date2), (#3 date1, #3 date2)];
      fun is_older_recursive (nums: (int*int) list) =
	if null nums
	then false
	else if #1 (hd(nums)) = #2 (hd(nums))
	then is_older_recursive(tl(nums))
	else #1 (hd(nums)) < #2 (hd(nums))
  in
      is_older_recursive(dates)
  end

fun number_in_month (dates: (int*int*int) list, month: int) =
  if null dates
  then 0
  else if #2 (hd(dates)) = month
  then 1 + number_in_month (tl(dates), month)
  else number_in_month(tl(dates), month)

fun number_in_months (dates: (int*int*int) list, months: int list) =
  if null months
  then 0
  else number_in_month(dates, hd(months)) + number_in_months(dates, tl(months))
		
fun dates_in_month (dates: (int*int*int) list, month: int) =
  if null dates
  then []
  else if #2 (hd(dates)) = month
  then hd(dates) :: dates_in_month(tl(dates), month)
  else dates_in_month(tl(dates), month)

fun dates_in_months (dates: (int*int*int) list, months: int list) =
  if null months
  then []
  else dates_in_month(dates, hd(months)) @ dates_in_months(dates, tl(months))

fun get_nth (xs: 'a list, n: int) =
  if n = 1
  then hd(xs)
  else get_nth (tl(xs), n-1)

fun date_to_string (date: int*int*int) =
  let
      val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
  in
      get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

fun number_before_reaching_sum (sum: int, elements: int list) =
  if null elements orelse sum <= hd(elements)
  then 0
  else 1 + number_before_reaching_sum(sum - hd(elements), tl(elements))

fun what_month (day: int) =
  let
      val month_lengths = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30];
  in
      number_before_reaching_sum(day, month_lengths)
  end

fun month_range (day1: int, day2: int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1+1, day2)

fun oldest (dates: (int*int*int) list) =
  if null dates
  then NONE
  else
      let
	  fun oldest_nonempty (dates: (int*int*int) list) =
	    if null (tl dates)
	    then hd dates
	    else
		let
		    val oldest_date = oldest_nonempty(tl(dates))
		 in
		     if is_older(hd(dates), oldest_date)
		     then hd dates
		     else oldest_date
		end
      in
	  SOME (oldest_nonempty dates)
      end

fun remove_duplicate_months (months: int list) =
  if null months
  then []
  else
      let
	  val non_duplicate_months = remove_duplicate_months(tl(months))
	  fun find (month: int, check_months: int list) =
	    if null check_months
	    then false
	    else if month = hd check_months
	    then true
	    else find(month, tl(check_months))
      in
	  if find(hd(months), non_duplicate_months)
	  then non_duplicate_months
	  else hd(months) :: non_duplicate_months
      end

fun number_in_months_challenge (dates: (int*int*int) list, months: int list) =
  let
      val non_duplicate_months = remove_duplicate_months(months);
  in
      number_in_months(dates, non_duplicate_months)
  end
  
fun dates_in_months_challenge (dates: (int*int*int) list, months: int list) =
  let
      val non_duplicate_months = remove_duplicate_months(months);
  in
      dates_in_months(dates, non_duplicate_months)
  end

fun reasonable_date (date: (int*int*int)) =
  let
      val is_leap_year =
	  (#1 date) mod 400 = 0 orelse ((#1 date) mod 4 = 0 andalso (#1 date) mod 100 <> 0);
	    
      val calendar_days = if is_leap_year
			  then [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
			  else [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  in
      
      #1 date > 0 andalso #2 date > 0 andalso #2 date <= 12 andalso #3 date > 0 andalso #3 date <= get_nth(calendar_days,(#2 date))
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
