type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail

fun pass_or_fail {id : student_id, grade: grade option} =
  case grade of
      NONE => fail
   | SOME i=> if(i >=75 ) then pass else fail
      
fun has_pass (grade: final_grade) =
  case pass_or_fail(grade) of
      pass => true
   | fail => false
  
      
fun number_passed(grades:final_grade list) =
  case grades of
      [] => 0
    | x::xs' => if has_pass(x)
		then 1 + number_passed(xs')
		else number_passed(xs')




				  (* test *)
val grade1 = {id = 1, grade = SOME 58};
val grade2 = {id = 2, grade = SOME 61};
val grade3 = {id = 3, grade = SOME 90};
val grade4 = {id = 1, grade = NONE};

val grades = [grade1,grade2,grade3,grade4];

val test3 = number_passed(grades) = 1;
				 
				  
