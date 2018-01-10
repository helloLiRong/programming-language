(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

(*Exercise 1.a*)
fun all_except_option(s, ls) =
    let fun find(ls) =
            case ls of
              [] => false
            | x::xs => if same_string(s, x) then true else find(xs)
        fun build_list(ls) =
            case ls of
              [] => []
            | x::xs => if same_string(x,s) then build_list xs
                                           else x::build_list xs
    in
      if find(ls) then SOME (build_list ls)
                  else NONE
    end

(* Exercise 1.b *)
 fun get_substitutions1(subs, s) =
    case subs of
        [] => []
      | x::xs => case all_except_option(s,x) of
                   SOME x => x @ get_substitutions1(xs, s)
                 | NONE => get_substitutions1(xs, s)

(* Exercise 1.c *)
fun get_substitutions2(subs, s) =
  let
      fun helper(subs, acc) =
          case subs of
            [] => acc
          | x::xs => case all_except_option(s,x) of
                      SOME x' => helper(xs, acc @ x')
                    | NONE => helper(xs, acc)
  in
     helper(subs,[])
  end

type full_name = {first:string,middle:string,last:string}

(* Exercise 1.d*)
fun similar_names(subs,ns) =
  let fun build_similar(substituted) =
          case (substituted,ns) of
            ([],_) => []
          | (x::xs,{first=_,last=l,middle=m}) => {first =x, last=l ,middle=m}::(build_similar xs)
  in
    case ns of
      {first= f,last=_,middle=_} => ns :: build_similar(get_substitutions2(subs,f))
  end

(* Exercise 2.a*)
fun card_color(card) =
  case card of
    (Clubs,_) => Black
  | (Spades,_) => Black
  | (Diamonds,_) => Red
  | (Hearts,_) => Red

(* Exercise 2.b*)
fun card_value(card) =
  case card of
    (_,Num x) => x
  | (_,Ace) => 11
  | _ => 10

(* Exercise 2.c*)
fun remove_card(cs,card,e) =
  let fun contains_card(cs) =
          case cs of
            [] => false
          | x::xs => if card = x then true else contains_card(xs)
      fun remove(cs,removed) =
          case cs of
            [] => []
          | c::cs => if (card = c) andalso not removed
                     then remove(cs,true)
                     else c::remove(cs,removed)
  in
      if contains_card(cs)
      then remove(cs,false)
      else raise e
  end

(* Exercise 2.d*)
fun all_same_color(cs)=
    case cs of
      [] => true
    | _::[] => true
    | head::(neck::xs)  =>  card_color(head) = card_color(neck) andalso all_same_color(neck::xs)

(* Exercise 2.d*)
fun sum_cards(cs) =
    let fun sum_acc(cs,acc) =
            case cs of
              [] => acc
            | c::cs => sum_acc(cs,card_value(c)+acc)
    in
        sum_acc(cs,0)
    end

fun score(cs,goal) =
  let val sum = sum_cards(cs)
      val preliminary = if sum > goal then 3 * (sum - goal) else goal - sum
  in
    if all_same_color cs
    then preliminary div 2
    else preliminary
  end

fun officiate(cs,moves,goal) =
  let fun run(cs,moves,held) =
          case (cs,moves) of
            (_,[]) => score(held,goal)
          | ([],Draw::ms) => score(held,goal)
          | (c::cs',Draw::ms) => if sum_cards(c::held) > goal
                                 then score(c::held,goal)
                                 else run(cs',ms,c::held)
          | (c::cs',(Discard card)::ms) => let val held_after_remove = remove_card(held,card,IllegalMove)
                                           in run(cs,ms,held_after_remove)
                                           end
    in
      run(cs,moves,[])
    end