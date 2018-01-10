(* Exercise 1 *)
fun is_older(x:int * int * int, y:int * int * int) =
      if (#1 x) < (#1 y)
      then true
      else if (#1 x) = (#1 y) andalso (#2 x) < (#2 y)
            then true
            else (#1 x) = (#1 y) andalso (#2 x) = (#2 y) andalso (#3 x) < (#3 y)

(* Exercise 2 *)
fun number_in_month(xs: (int * int * int) list, month: int ) =
      let
        fun month_count(d: int * int * int) =
          if (#2 d) = month
          then 1
          else 0  
      in
         if null xs
         then 0
         else month_count(hd xs) + number_in_month(tl xs,month)
      end

(* Exercise 3 *)
fun number_in_months(xs: (int * int * int) list, months: int list ) =
      if null months
      then 0
      else number_in_month (xs, hd months) + number_in_months(xs,tl months)

(* Exercise 4 *)
fun dates_in_month(xs: (int * int * int) list,month: int) =
        if null xs
        then []
        else let
                val filterDates = dates_in_month(tl xs,month)
             in
               if month = #2 (hd xs)
               then (hd xs)::filterDates
               else filterDates
             end

(* Exercise 5 *)
fun dates_in_months(xs: (int * int * int) list,months: int list) =
      if null months
      then []
      else dates_in_month(xs,hd months) @ dates_in_months(xs,tl months)

(* Exercise 6 *)
fun get_nth(xs: string list,n: int) =
    let
      fun get_nth_counter(xs: string list, c:int) =
          if n = c
          then hd xs
          else get_nth_counter(tl xs, c+1)
    in
      get_nth_counter(xs,1)
    end

(* Exercise 7     *)
fun date_to_string(xs: (int * int * int)) =
    let
      val month = get_nth(["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"],
                          #2 xs )
    in
      month ^ " " ^ Int.toString (#3 xs) ^ ", " ^ Int.toString (#1 xs)
    end

(* Exercise 8      *)
fun number_before_reaching_sum(sum: int, xs: int list) =
    let
      fun sum_counter(s:int,c:int, xs: int list) =
          if null xs
          then c
          else let val acc = s + hd xs
                in
                  if acc >= sum
                  then c
                  else sum_counter(acc, c+1, tl xs)
                end
    in
      sum_counter(0,0,xs)
    end

(* Exercise 9*)
fun what_month(diy: int)=
    let
      val ranges = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
      number_before_reaching_sum(diy,ranges) + 1
    end

(* Exercise 10*)
fun month_range(d1:int, d2:int) =
    if d1 > d2
    then []
    else what_month d1::month_range(d1+1,d2)

(* Exercise 11*)
fun oldest(xs: (int * int * int) list) =
    let
      fun find_older(date:int * int * int, xs: (int * int * int) list)=
          if null xs
          then date
          else let
                  val new_date = if is_older(date,hd xs)
                                 then date
                                 else hd xs
               in
                  find_older(new_date,tl xs)
               end
    in
      if null xs
      then NONE
      else SOME(find_older(hd xs, tl xs))
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
