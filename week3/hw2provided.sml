(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* A *)
fun all_except_option (x, xs) =
  let fun find(xs, cnt, all) =
	case xs of
	    [] => (cnt, all)
	  | hd1::tl1 => if same_string(hd1, x)
			then find(tl1, cnt+1, all)
			else find(tl1, cnt, hd1::all)
  in
      case find(xs,0,[]) of
	  (0,_) => NONE
	| (_,lst) => SOME(lst)
  end

(* B *)
fun get_substitutions1(subs, s) =
  case subs of
      [] => []
    | cur::lst => case all_except_option(s, cur) of
		      SOME res => res @ get_substitutions1(lst, s)
		    | _ => get_substitutions1(lst, s)
					     
							  
(* C *)
fun get_substitutions2(subs, s) =
  let fun aux(subs) =
	case subs of
	    [] => [] 
	  | cur::lst  => case all_except_option(s, cur) of
			     SOME res => res @ aux(lst)
			  | _ => aux(lst) 
  in
      aux(rev subs)
  end

(* D *)
fun similar_names(lst_names, name) =
  let fun aux(subs, acc) =
	case subs of
	    [] => name :: acc 
	  | hd1::tl1  => let val n = {first = hd1, middle = (#middle name), last = (#last name)}
			 in
			     aux(tl1, n :: acc)
			 end
  in
      aux(get_substitutions2(lst_names, #first name), [])
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
(* 2 *)

fun card_color (card) =
  case card of
      (Clubs, _) => Black
    | (Spades,_) => Black
    | _ => Red
	      
fun card_value (card) =
  case card of
      (_, Ace) => 11
   |  (_, Num(x)) => x
   |  (_,_)  => 10

	    
fun remove_card (cs, c, e) =
  let fun rem(cs, cnt, all) =
	case cs of
	    [] => (cnt, all)
	  | hd1::tl1 => if hd1 = c
			then rem([], cnt+1, all @ tl1)
			else rem(tl1, cnt, hd1::all)
  in
      case rem(cs,0,[]) of
	  (0,_) => raise e
	| (_,lst) => lst
  end

fun same_color (c1, c2) =
  card_color(c1) = card_color(c2)
      
fun all_same_color (cs) =
  case cs of
      [] => true
    | _::[]  => true 
    | card1::(card2::others) => (same_color(card1, card2)
			  andalso all_same_color (card2::others))
					   
fun sum_cards (cs) =
  let fun sum(cs, acc) =
	case cs of
	    [] => acc
	  | card1::others => sum(others, card_value(card1) + acc)
  in
      sum(cs,0)
  end
      
fun score (cs, goal) =
  let fun pre_score (sum) =
	case sum > goal of
	    true => 3 * (sum - goal)
	  | _ => (goal - sum)
  in
      let val ps = pre_score(sum_cards(cs)) in
	  case all_same_color(cs) of
	      true => ps div 2
	   | _ => ps
      end
  end

fun officiate(cl, ml, g) =
  let fun state(cl, ml, hl) =
      case (cl, ml, score(hl,g)) of
	  ([],_, sc) => sc
	| (_,[], sc) => sc
	| (cl, Discard(c)::omv,sc) =>
	  state(remove_card (cl, c, IllegalMove), omv, hl)
	| (c::ocl, Draw::omv, sc) =>
	  (if sc > g
	   then sc
	   else state(ocl, omv, c::hl)) 
  in
      state(cl, ml, [])
  end
