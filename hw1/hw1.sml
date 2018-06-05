(* @author: Dingze Wang *)
(* Functional Programming homework1 *)

(* problem1 is_older *)
fun is_older (date1: int*int*int, date2: int*int*int) = 
	let fun countup (num: int*int*int) = 
		10000 * (#1 num) + 100 * (#2 num) + (#3 num)
	in
		if countup(date1) < countup(date2)
		then true
		else false
	end


(* problem2 how many dates in the list are in the given month *)
fun number_in_month (dates: (int * int * int) list, month: int) =
	let fun valid (date: (int * int * int), m: int) = 
		if (#2 date = m)
		then 1
		else 0
	in
		if null dates
		then 0
		else valid(hd dates, month) + number_in_month (tl dates, month)
	end


(* problem3 number of dates in the list of dates that are in any of the months in the list of months. *)
fun number_in_months (dates: (int * int * int) list, months: int list) = 
	if null months
	then 0
	else number_in_month(dates, hd months) + number_in_months(dates, tl months)


(* problem4 a list holding the dates from the argument list of dates that are in the month. *)
fun dates_in_month (dates: (int * int * int) list, month: int) = 

	if null dates
	then []
	else
		let fun valid (date: (int * int * int), m: int) = 
			if (#2 date = m)
			then true
			else false
		in
			if valid(hd dates, month)
			then hd dates :: dates_in_month(tl dates, month)
			else dates_in_month(tl dates, month)
		end

(* problem5 a list holding the dates from the argument list of dates that are in any of the months in
the list of months. *)
fun dates_in_months (dates: (int * int * int) list, months: int list) = 
	if null months
	then []
	else
		dates_in_month(dates, hd months) @ dates_in_months (dates, tl months)


(* problem6 returns the nth element of the list where the head of the list is 1st *)
fun get_nth (strings: string list, index: int) = 
	if index = 1
	then hd strings
	else 
		get_nth(tl strings, index - 1)

(* problem7 takes a date and returns a string of the form January 20, 2013 (for example) *)
fun date_to_string (date: int * int * int) = 
	let val months = ["January ", "February ", "March ", "April ",
		"May ", "June ", "July ", "August ", "September ", "October ", "November ", "December "]
	in 
		get_nth(months, (#2 date)) ^ Int.toString((#3 date)) ^ ", " ^ Int.toString((#1 date))
	end

(* problem8 First n elements of the list add to less than sum, but the First
n+1 elements of the list add to sum or more. *)
fun number_before_reaching_sum (sum: int, union: int list) = 
	if hd union >= sum
	then 0
	else 1 + number_before_reaching_sum (sum - hd union, tl union)


(* problem9 what month that day is in *)
fun what_month (day: int) = 
	let val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in
		number_before_reaching_sum (day, months) + 1
	end


(* problem10 the month range of day1 and day2 *)
fun month_range (day1: int, day2: int) = 
	if (day1 > day2)
	then []
	else
		what_month(day1) :: month_range (day1 + 1, day2)


(* problem11 the oldest day in the list, if null list return null *)
fun oldest (dates : (int * int * int) list) = 
	if null dates
    then NONE
    else 
    	let 
    		fun countup (num: int*int*int) = 
				10000 * (#1 num) + 100 * (#2 num) + (#3 num)
		in
			let val tl_oldest = oldest(tl dates)
    		in 
    			if isSome tl_oldest andalso countup(hd dates) > countup(valOf tl_oldest)
    			then tl_oldest
    			else SOME(hd dates)
    		end
		end


(* helper function to remove duplicated keys *)
fun remove_duplicated_keys(months: int list) = 
	let fun isMember (month: int, months: int list) = 
		if null months
		then false
		else
			if month = hd months
			then true
			else isMember(month, tl months)
	in
		if null months
		then []
		else 
			let val restMonths = remove_duplicated_keys(tl months)
			in 
				if not (isMember(hd months, restMonths))
				then hd months :: restMonths
				else restMonths
			end 
	end



(* problem12 problem3 with duplicated keys *)
fun number_in_months_challenge (dates: (int * int * int) list, months: int list) =
	number_in_months (dates, remove_duplicated_keys(months))

(* problem12 problem5 with duplicated keys *)
fun dates_in_months_challenge (dates: (int * int * int) list, months: int list) = 
	dates_in_months (dates, remove_duplicated_keys(months))


(* helper function for problem 13 
	judge if a year is leap year*)
fun is_leap_year (year: int) = 
	if (year mod 400) = 0
	then true
	else
		if (year mod 100) = 0
		then false
		else 
			if (year mod 4) = 0
			then true
			else false

(* helper function for problem 13 
	judge if a year is valid year*)
fun is_valid_year (year: int) = 
	if year <= 0
	then false
	else true

(* helper function for problem 13 
	judge a if month is valid month *)
fun is_valid_month (month: int) = 
	if month > 12 orelse month < 1
	then false
	else true

(* helper function for problem 13 
	judge a if month is valid date *)
fun is_valid_date (year: int, month: int, date: int) = 
	if month = 2
	then 
		if (is_leap_year(year))
		then if date <= 29 andalso date >= 1
			then true
			else false
		else if date <= 28 andalso date >= 1
			then true
			else false
	else 
		let val months = ["31", "28", "31", "30", "31", "30", "31", "31", "30", "31", "30", "31"]
			val upperBound = get_nth(months, month)
		in
			if (date <= valOf(Int.fromString upperBound)) andalso (date >= 1)
			then true
			else false
		end


(* problem13 if a date is reasonable *)
fun reasonable_date (date: int*int*int) = 
	if (not (is_valid_year(#1 date)))
	then false
	else
		if (not (is_valid_month(#2 date)))
		then false
		else
			if (not (is_valid_date(#1 date, #2 date, #3 date)))
			then false
			else true

