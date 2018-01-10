fun is_older (date1 : int * int * int , date2 : int * int * int) =
  if (#1 date1) = (#1 date2)
  then
      if (#2 date1) = (#2 date2)
      then
	  if (#3 date1) < (#3 date2)
	  then true
	  else false		   
      else (#2 date1) < (#2 date2)			    
  else (#1 date1) < (#1 date2)
			

		   
fun number_in_month (dateList : (int*int*int) list,month : int) =
  if null dateList
  then 0
  else
      if (#2 (hd dateList) = month)
      then 1 + number_in_month(tl dateList, month)
      else  number_in_month(tl dateList, month)
			   
	  
fun number_in_months (dateList : (int*int*int) list, months : int list) =
  if null months
  then 0
  else number_in_month(dateList, hd months) + number_in_months(dateList,tl months)

fun dates_in_month (dateList : (int*int*int) list, month : int) =
  if null dateList
  then []
  else
      if (#2 (hd dateList)) = month
      then hd dateList :: dates_in_month (tl dateList, month)
      else dates_in_month (tl dateList, month)	  		  

fun dates_in_months (dateList : (int*int*int) list, months : int list)=
  if null months
  then []
  else dates_in_month(dateList, hd months) @ dates_in_months(dateList,tl months)
							     
						     
fun get_nth (strings : string list, n : int) =
  if n = 1
  then hd strings
  else get_nth(tl strings,n-1)

fun date_to_string (date : (int*int*int) ) =
  let val months = ["January","February","March","April","May","June","July","August","September","October","November","December"]
  in
      get_nth(months,(#2 date)) ^ " " ^ Int.toString((#3 date)) ^ ", " ^ Int.toString((#1 date))
  end

fun number_before_reaching_sum (sum : int , numbers : int list) =
  if sum <= hd numbers
  then 0
  else
      1 + number_before_reaching_sum (sum - (hd numbers) , tl numbers)

fun what_month (day : int) =
  let val days = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
      number_before_reaching_sum(day,days) + 1
  end
      
fun month_range (day1 : int , day2 : int) =
  if day1 > day2
  then []
  else
      what_month day1 :: month_range( day1 + 1, day2)

fun oldest (dates : (int * int * int) list) =
  if null dates
  then NONE
  else
      let
	  fun oldest_noempty (dates : (int * int * int ) list) =
	    if null (tl dates)
	    then hd dates
	    else
		let val max_tail_date = oldest_noempty (tl dates)
		in
		    if is_older ((hd dates),max_tail_date)
		    then hd dates 
		    else max_tail_date
		end
      in
	  SOME(oldest_noempty(dates))
      end
	  
