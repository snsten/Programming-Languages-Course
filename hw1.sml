(* Assignment 1 *)

fun is_older (date1 : int*int*int, date2 : int*int*int) : bool =
    if #1 date1 < #1 date2 then true
    else if #1 date1 > #1 date2 then false
    else if #2 date1 < #2 date2 then true
    else if #2 date1 > #2 date2 then false
    else if #3 date1 < #3 date2 then true
    else false

fun number_in_month (dates : (int * int * int) list, month : int)  =
    if null dates
    then 0
    else if (#2 (hd dates)) = month
    then 1 + number_in_month((tl dates), month)
    else 0 + number_in_month((tl dates), month)
			    
fun number_in_months (dates : (int * int * int) list, month : int list) =
    if null month
    then 0
    else number_in_month (dates, (hd month)) + number_in_months ((dates), (tl month))

fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else if (#2 (hd dates)) = month
    then (hd dates) :: dates_in_month((tl dates), month)
    else dates_in_month((tl dates), month)


fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, (hd months)) @ dates_in_months(dates, (tl months))


fun get_nth (xs : string list, n : int) =
    if n = 1
    then hd xs
    else get_nth(tl xs, n-1)

fun date_to_string (date : int * int * int) =
    let
	val months = ["January", "February", "March", "April", "May", "June", "July",
		      "August", "September", "October","November", "December"]
    in
	get_nth(months, (#2 date)) ^ " " ^ Int.toString((#3 date)) ^ ", " ^ Int.toString((#1 date))
    end
	
fun number_before_reaching_sum (sum : int, xs : int list) =
    let
	fun reach_sum_index (xs : int list, sum : int, n : int) =
	    if sum < 1
	    then n-1
	    else reach_sum_index(tl xs, sum - (hd xs), n+1)
    in
	reach_sum_index(xs, sum, 0)
    end


fun what_month (day : int) =
    let
	val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum(day, days_in_months) + 1
    end
	
			 
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1+1, day2)


fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else
	let
	    fun older (dates: (int * int * int) list, date: (int * int * int)) =
	    if null dates
	    then SOME date
	    else
		if is_older(date, hd dates)
		then older(tl dates, date)
		else older(tl dates, hd dates)
	in
	    older(tl dates, hd dates)
	end
