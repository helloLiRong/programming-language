(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* problem a *)
fun all_except_option(strs, strsList) =
  let fun helper(tempList, acc) =
	case (tempList, acc) of
	   (x::xs', SOME l) => if(same_string(x, strs))
				then SOME (l@xs')
				else helper(xs',SOME (x::l))
	  | _ => NONE
		     
  in
      helper(strsList,SOME [])
  end
      
(* probelm b *)
fun get_substitutions1(namesList, name) =
  case namesList of
      [] => []
    | x::xs' => case all_except_option(name,x) of
		    NONE => get_substitutions1(xs',name)
		 |  SOME l => l @ get_substitutions1(xs',name)
						   
(* problem c*)  
fun get_substitutions2(namesList, name) =
  let fun aux(tempList, acc) =
	case tempList of
	    [] => acc
	  | x::xs' => case all_except_option(name,x) of
			  NONE => aux(xs',acc)
			| SOME l => aux(xs',l @ acc)
  in
      aux(namesList,[])
  end
      

(* problem d *)
fun similar_names(namesList, {first=x,middle=y,last=z}) =
  let val similar_first_list = get_substitutions1(namesList,x)						 
      fun substit_full_name(temp)=
	case temp of
	    [] => []
	  | x::xs' => {first=x,middle=y,last=z} :: substit_full_name(xs')						  
  in
      {first=x,middle=y,last=z}::substit_full_name(similar_first_list)
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
	      
(* problem a *)	      
fun card_color(c) =
  case c of
      (Clubs,_) => Black
    | (Spades,_) => Black
    | (Diamonds,_) => Red
    | (Hearts,_) => Red

(* problem b *)			
fun card_value(c) =
  case c of
      (_,Num i) => i
    | (_,Ace) => 11
    | _ => 10

(* problem c *)	       
fun remove_card(cards, c, e) =
  case cards of
      [] => raise e
    | x::xs' => if(x = c)
		then xs'
		else x::remove_card(xs',c,e)

(* problem d *)				   
fun all_same_color(cards) =
  case cards of
      [] => true
    | a::[] => true
    | a::b::xs' => (card_color a = card_color b) andalso all_same_color(b::xs')

(* problem e *)
fun sum_cards(cards) =
  let fun aux(xs,acc) =
	case xs of
	    [] => acc
	  | x::xs' => aux(xs', acc + card_value(x))
  in
      aux(cards,0)
  end

(* problem f *)      
fun score(cards, goal) =
  let val sum = sum_cards(cards)
      val preliminary_score = if sum > goal
			      then 3 * (sum - goal)
			      else (goal - sum)
  in
      if all_same_color(cards)
      then preliminary_score div 2
      else preliminary_score	       
  end
      
(* problem g *)					     
fun officiate(cards, moves, goal) =
  let fun helper(held_cards, cards_left, moves_left) =
	case (moves_left, cards_left) of
	    ([], _) => held_cards
	  | (Draw::xs', []) => held_cards
	  | (Draw::xs', c::c') => if sum_cards(c::held_cards) <= goal
				  then helper(c::held_cards, c', xs')
				  else c::held_cards
	  | (Discard c::xs', _) => helper(remove_card(held_cards, c, IllegalMove), cards_left, xs')
      val held_cards = helper([], cards, moves)			     
  in
      score(held_cards,goal)
  end
