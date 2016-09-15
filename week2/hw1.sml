
fun is_older(t1 : int * int * int, t2 : int * int * int) =
  ((#1 t1) <= (#1 t2)) andalso ((#2 t1) <= (#2 t2)) andalso ((#3 t1) < (#3 t2))

fun in_month(date : (int * int * int), month : int) = ((#2 date) = month)
			    
fun number_in_month(dates : (int * int * int) list, month : int) =
  if null dates
  then 0
  else if in_month((hd dates), month)
  then 1 + number_in_month(tl dates, month)
  else number_in_month(tl dates, month)
	   
			      
fun number_in_months(dates : (int * int * int) list, months : int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

							   
fun dates_in_month(dates : (int * int * int) list, month) =
  if null dates
  then []
  else if in_month((hd dates), month)
  then (hd dates) :: dates_in_month(tl dates, month)
  else dates_in_month(tl dates, month)
			 
fun dates_in_months(dates : (int * int * int) list, months : int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strings : string list, pos : int) = let
    fun next_nth(items : string list, cur : int) =
      if cur = pos
      then hd items
      else next_nth(tl items, cur + 1)
in
    next_nth(strings, 1)
end
      
fun date_to_string(date : (int * int * int)) = let
    val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
    fun month_name(months : string list, cur : int) =
      if cur = (#2 date)
      then hd months
      else month_name(tl months, cur + 1)
in
    month_name(months,1) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
end


fun number_before_reaching_sum(sum : int, numbers : int list) = let
    fun count(numbers : int list,  acc : int, pos: int) =
      if (acc + hd(numbers)) >= sum
      then pos
      else count(tl numbers, acc + hd(numbers), pos + 1)
in
    count(numbers, 0, 0)
end

fun what_month(days : int) = let
    val limits = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
in
    number_before_reaching_sum(days, limits) + 1
end
				 
fun month_range(day1 : int, day2 : int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1+1, day2)

				      (*
fun oldest(dates : (int * int * int) list) =
  if null dates
  then NONE
  else let
      fun oldest_non_empty(dates : (int * int * int) list) =
	if null (tl dates)
	then hd dates
	else let val tl_oldest = oldest_non_empty(tl dates)
	     in
		 if is_older(hd dates, tl_oldest)
		 then hd dates
		 else tl_oldest
	     end
  in
      SOME(oldest_non_empty dates)
  end
*)	   
