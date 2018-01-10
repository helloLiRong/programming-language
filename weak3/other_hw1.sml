(* Dan Grossman, Coursera PL, HW2 Provided Code *)
(* 
    @Description: Programming Languages Part A week 3 homework
    @Author: sawyer
    @Date: 2017/12/27
*)

(* Problem 1 *)
(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(target, lst) = 
    let 
        fun rail_recursive(curList, accList) = 
            case curList of 
                [] => NONE
                | x :: x' => 
                    if same_string(target, x) then SOME (accList @ x')
                    else rail_recursive(x', x :: accList)  
    in
        rail_recursive(lst, [])
    end


fun get_substitutions1(substitutions, target) = 
    case substitutions of 
        [] => []
        | x :: x' =>
            case all_except_option(target, x) of
                NONE => get_substitutions1(x', target)
                | SOME y => y @ get_substitutions1(x', target)


fun get_substitutions2(substitutions, target) = 
    let
        fun rail_recursive(curList, accList) = 
            case curList of
                [] => accList
                | x :: x' => 
                    case all_except_option(target, x) of
                        NONE => rail_recursive(x', accList)
                        | SOME y => rail_recursive(x', y @ accList)
    in
        rail_recursive(substitutions, [])
    end


fun similar_names(substitutions, {first = f, middle = m, last =  l}) = 
    let
        val firstNames = get_substitutions2(substitutions, f)
    in
        let
            fun substitute(firstNames, accList) = 
                case firstNames of
                    [] => accList
                    | x :: x' => substitute(x', {first = x, middle = m, last = l} :: accList)
        in
            substitute(firstNames, [{first = f, middle = m, last =  l}])
        end
    end

(* Problem 2 *)
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (color, _) =
    case color of
        Clubs => Black
        | Diamonds => Red
        | Hearts => Red
        | Spades => Black


fun card_value(_, value) = 
    case value of 
        Num x => x
        | Ace => 11
        | _ => 10


fun remove_card(cs, c, e) =
    let
         fun remove(curCards, accCards) = 
            case curCards of 
                [] => raise e
                | x :: x' => 
                    if x = c then accCards @ x'
                    else remove(x', x :: accCards)
    in
        remove(cs, [])
    end


fun all_same_color(cards) = 
    case cards of 
        [] => true
        | _ :: [] => true
        | head :: (neck :: rest) => (card_color(head) = card_color(neck) andalso all_same_color(neck :: rest))


fun sum_cards(cards) =
    let
        fun rail_recursive(curCards, acc) =
            case curCards of
                [] => acc
                | x :: x' => rail_recursive(x', acc + card_value(x))
    in
        rail_recursive(cards, 0)
    end


fun score(cards, goal) =
    let 
        val sum = sum_cards(cards)
        val pre_score = 
            if sum > goal then 3 * (sum - goal)
            else goal - sum
    in
        if all_same_color(cards) then pre_score div 2
        else pre_score
    end

fun officiate(cardList, moveList, goal) =
    let
        fun play(heldCards, moveList, restCards) = 
            case moveList of
                [] => score(heldCards, goal)
                | m :: ms' => 
                    case m of
                        Discard m => play(remove_card(heldCards, m, IllegalMove), ms', restCards)
                        | Draw =>
                            case restCards of
                                [] => score(heldCards, goal)
                                | c :: rc' =>
                                    if sum_cards(c :: heldCards) > goal then score(heldCards, goal)
                                    else play(c :: heldCards, ms',  rc')
    in
        play([], moveList, cardList)
    end

(* Challenge Problems *)
fun card_value2(_, value) = 
    case value of
        Num x => x
        | Ace => 1
        | _ => 10

fun sum_cards2(cards) =
    let
        fun aux_sum(curCards, acc) =
            case curCards of
                [] => acc
                | x :: x' => aux_sum(x', acc + card_value(x))
        fun aux_sum2(curCards, acc) =
            case curCards of
                [] => acc
                | x :: x' => aux_sum2(x', acc + card_value2(x))
        val max_sum = aux_sum(cards, 0)
        val min_sum = aux_sum2(cards, 0)
    in
        (max_sum, min_sum)
    end

fun score_challenge(cards, goal) = 
    let
        fun calc(sum) =
            if sum > goal then 3 * (sum-goal)
            else goal - sum

        val (max_sum, min_sum) = sum_cards2(cards)
        val pre_score = Int.min(calc(max_sum), calc(min_sum))
    in
         if all_same_color(cards) then pre_score div 2
        else pre_score
    end


fun officiate_challenge(cardList, moveList, goal) =
    let
        fun play(heldCards, moveList, restCards) = 
            case moveList of
                [] => score_challenge(heldCards, goal)
                | m :: ms' => 
                    case m of
                        Discard m => play(remove_card(heldCards, m, IllegalMove), ms', restCards)
                        | Draw =>
                            case restCards of
                                [] => score_challenge(heldCards, goal)
                                | c :: rc' =>
                                    if sum_cards(c :: heldCards) > goal then score_challenge(heldCards, goal)
                                    else play(c :: heldCards, ms',  rc')
    in
        play([], moveList, cardList)
    end

(* 
officiate: Your function returns an incorrect result when the game should end due to the sum of cards in the player's hand exceeding the goal. [incorrect answer]
officiate_challenge: Your function returns an incorrect result when an ace is in the players hand. [incorrect answer]
officiate_challenge: Your function returns an incorrect result when the game should end due to an empty move list with low score. [incorrect answer]
3b tests failed to run (most likely caused by an incorrect function signature or unimplemented function in the submission)
*)
