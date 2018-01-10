(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(str, []) = NONE
    |  all_except_option(str, x::xs) =
        case(same_string(str, x), all_except_option(str, xs)) of
        (true, _) => SOME(xs)
        |   (false, NONE) => NONE
        |   (false, SOME(y)) => SOME(x::y)

fun get_substitutions1 ([], _) = []
    |  get_substitutions1 (x::xs, str) =
        case all_except_option(str, x) of
        NONE => get_substitutions1(xs, str)
        |   SOME(y) => y @ get_substitutions1(xs, str)

fun get_substitutions2 ([], _) = []
    | get_substitutions2 (lst, str) = 
        let fun helper_fcn(xs, str, acc) =
            case xs of
            [] => acc
            | x::xs' => case all_except_option(str, x) of
                            NONE => helper_fcn(xs', str, acc)
                            |   SOME(y) => helper_fcn(xs', str, acc @ y)
        in
            helper_fcn(lst, str, [])
        end
fun similar_names (xs, {first, middle, last}) =
    let fun helper_fun([], acc) = acc |
        helper_fun(f::xs', acc) =
            helper_fun(xs', acc @ [{first = f, middle=middle, last=last}])

    in helper_fun(first::get_substitutions1(xs,first), [])
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
    fun card_color(c: card) = 
        case c of
            (Clubs, _) => Black
            |   (Spades, _) => Black
            |   (Diamonds, _) => Red
            |   (Hearts, _) => Red

    fun card_value(c: card) = 
        case c of
            (_, Ace) => 11
            | (_, King) => 10
            | (_, Queen) => 10
            | (_, Jack) => 10
            | (_, Num num) => num


    fun remove_card([], _, e) = raise e
    |   remove_card(c::cs, card, e) =
        case c = card of
            true => cs
            |   false => c::remove_card(cs, card, e)

    fun all_same_color([]) = false
        |   all_same_color(c::cs) =
        case cs of
            [] => true
            |   c'::cs' => if card_color(c) = card_color(c')
                           then all_same_color(cs)
                           else false

    fun sum_cards(cs) =
        let fun helper_fcn(cs, acc) =
            case cs of
                [] => acc
                |   c::cs' => acc + card_value(c) + helper_fcn(cs', acc)
        in
            helper_fcn(cs, 0)
        end

    fun score([], _) = 0
        |   score(cs, goal) = 
        let fun prelim(sum) =
                if sum > goal
                then  3 * (sum -goal)
                else (goal - sum)
        
            fun final(sum, same) =
                if same
                then sum div 2
                else sum 
        in
            final(prelim(sum_cards(cs)), all_same_color(cs))
        end


     fun officiate(cs, moves, goal) =
        let fun play_game(_, [], heldcards) = score(heldcards, goal) 
            |  play_game([], Draw::moves, heldcards) = score(heldcards, goal)
            |  play_game(cs, Discard card::moves, heldcards) = 
                    play_game(cs, moves, remove_card(cs, card, IllegalMove))
            |  play_game(c::cs', Draw::moves, heldcards) =
                    case sum_cards(c::heldcards) > goal of
                        true => score(c::heldcards, goal)
                        |   false => play_game(cs', moves, c::heldcards)
        
        in 
            play_game(cs, moves, [])
        end      
            
