	       
val days_per_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
val days_per_month_leap_year = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	  

(* =====================================================================
Functions							       *)	

	       
(* (int * int * int) * (int * int * int) -> bool
interp. returns True if first date is prior to second date, else False *)	
fun is_older (date1: int * int * int, date2: int * int * int) =
    if (#1 date1) < (#1 date2)
    then true
    else if (#1 date1) > (#1 date2)
    then false
    else if (#2 date1) < (#2 date2)
         then true
         else if (#2 date1) > (#2 date2)
         then false
         else (#3 date1) < (#3 date2)
	     


			       
(* (int * int * int) list * int -> int
interp. given a month and a list of dates, return number of dates in 
given month *)
fun number_in_month (lod : (int * int * int) list, month : int) =
    if null lod
    then 0
    else let fun number_in_month_nonempty (lod : (int * int * int) list, count : int) =
		 if null (tl lod)
		 then
		        if #2 (hd lod) = month
			then count + 1
			else count
		 else
		        if #2 (hd lod) = month
		        then number_in_month_nonempty (tl lod, count + 1)
		        else number_in_month_nonempty (tl lod, count)
									    			    
	 in
	     number_in_month_nonempty (lod, 0)
	 end
	     

					 
					 
(* (int * int * int) list * int list -> int
interp. given list of months and list of dates, return number of dates in the given months 
ASSUME: no months are repeated *)
fun number_in_months (lod : (int * int * int) list, lom : int list) =
    if null lom
    then 0
    else (number_in_month (lod, hd lom))
	     + (number_in_months (lod, tl lom))

		   


(* (int * int * int) list * int -> (int * int * int) list
interp. returns list of dates from the given month 
rsf is results so far accumulator *)			
fun dates_in_month (lod: (int * int * int) list, month : int) =
    if null lod
    then []
    else let
	fun dates_in_month_nonempty (lod : (int * int * int) list, rsf) =
		 if null (tl lod)
		 then if #2 (hd lod) = month
		      then (hd lod) :: rsf
		      else rsf
		 else if #2 (hd lod) = month
		      then dates_in_month_nonempty (tl lod, (hd lod) :: rsf)
		 else dates_in_month_nonempty (tl lod, rsf)

	fun reverse_list (lod : (int * int * int) list, rsf) =
		 if null lod
		 then rsf
		 else reverse_list (tl lod, (hd lod) :: rsf)
				   
	 in reverse_list (dates_in_month_nonempty (lod, []), [])
	 end
	      			   
		
									    

(* (int * int * int) list * int list -> (int * int * int) list
interp. returns list of dates from given list of dates which are in 
any of the months in the given list of months *)
fun dates_in_months (lod : (int * int * int) list, lom : int list) =
    if null lod orelse null lom
    then []
    else dates_in_month (lod, hd lom) @ dates_in_months (lod, tl lom)
							  

				       

(* string list * int -> string
interp. Given a list of strings, returns the nth element 
ASSUME list has enough elements, n is positive *)
fun get_nth (los : 'a list, n : int) =
    if n = 1
    then hd los
    else get_nth (tl los, n-1)

	    
			    
(* int * int * int -> string
interp. Given date (int * int * int), return string of the format "Month Day, Year" *)
fun date_to_string (date : int * int * int) =
    let
	val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	fun number_to_month (month_number : int, months : string list) =
	    if month_number = 1
	    then hd months
	    else number_to_month (month_number - 1, tl months)

	val day = Int.toString (#3 date)
        val year = Int.toString (#1 date)
    in		       
         number_to_month (#2 date, months) ^ " " ^ day ^ ", " ^ year					 
    end	



	
(* int * int list -> int
interp. return an int n such that the first n elements of the list add to 
less than the given sum, but the first n + 1 elements of the list add to sum or more
ASSUME: sum is positive, sum of list > sum, list is non-empty, list is all positive *)
fun number_before_reaching_sum (sum : int, loi : int list) =
    let
	fun number_before_reaching_sum_accumulator (loi : int list, sum_so_far : int, n : int) =
	    if sum_so_far + hd loi >= sum
	    then n 
	    else number_before_reaching_sum_accumulator (tl loi, sum_so_far + (hd loi), n + 1)
							    	       
    in
	number_before_reaching_sum_accumulator (loi, 0, 0)
    end
	


	
(* int -> int
interp. Given a day of the year, return what number month that day is in *)
fun what_month (day: int) =
    number_before_reaching_sum (day, days_per_month) + 1
	

			    
					       
(* int * int -> int list
interp. returns an int list [m1,m2,...,mn] where m1 is the month of day1,
 m2 is the month of day1+1, ..., and mn is the month of day2  
rsf is Result So Far accumulator *)
fun month_range (day1 : int, day2 : int) =
    if day2 < day1
    then []
    else
	let
	    fun month_range_helper (day1 : int, rsf : string list) =
		let
		    val day1_month_number = what_month (day1)
		    val day1_month = get_nth (months, day1_month_number)
		in   
		    if day1 = day2
		    then rsf @ [day1_month]
		    else month_range_helper (day1 + 1, rsf @ [day1_month])
		end
	in
	    month_range_helper (day1, [])
	end
	    

					    

(* (int * int * int) list -> (int * int * int) option
interp. Given a list of dates, return Option. NONE if list is empty, 
else SOME d if d is oldest date in list *)
fun oldest (lod : (int * int * int) list) =
    if null lod
    then NONE
    else
	let fun oldest_helper (lod : (int * int * int) list, current_oldest : int * int * int) =
		if null lod
		then current_oldest
		else if is_older (hd lod, current_oldest)
		then oldest_helper (tl lod, hd lod)
		else oldest_helper (tl lod, current_oldest)		   
	in
	    SOME (oldest_helper (lod, hd lod))
	end
	     

	    
(* int list -> int list *)
fun remove_duplicates (lom : int list) =
    if null lom
    then []
    else
	let
	    fun duplicates (month : int, lom : int list) =
		if null lom
		then false
		else if month = hd lom
		then true
		else duplicates (month, tl lom)
	in
	    if duplicates (hd lom, tl lom)
	    then remove_duplicates (tl lom)
	    else hd lom :: remove_duplicates(tl lom)
	end



		    
(* (int * int * int) list, int list -> int
interp. given list of dates and list of months, return number of
given dates in list of given months *)
fun number_in_months_challenge (lod : (int * int * int) list, lom : int list) =
    if null lod
    then 0
    else number_in_months (lod, remove_duplicates (lom))
    

	

(* (int * int * int) list, int list ->  (int * int * int) list
interp. returns list of dates from given list of dates which are in 
any of the months in the given list of months *)
fun dates_in_months_challenge (lod : (int * int * int) list, lom : int list) =
    dates_in_months (lod, remove_duplicates (lom))



		    
(* (int * int * int) -> Bool
interp. Given a date, returns True if date is real (positive year, valid month and day *) 
fun reasonable_date (date : int * int * int ) =
    let
	val year = #1 date
	val month = #2 date
	val day = #3 date

	fun leap_year () =
	    year mod 400 = 0 orelse
	    (year mod 4 = 0 andalso year mod 100 <> 0)
	fun reasonable_year () =
	    year > 0 andalso year < 2019
	fun reasonable_month () =
	    month > 0 andalso month < 13
	fun reasonable_day () =
	    let
		fun date_matches_month (days_per_month : int list) =
		    let
			val number_of_days_in_month = get_nth (days_per_month, month)
		    in
			number_of_days_in_month >= day
		    end
	    in	
		if not (leap_year ())
		then date_matches_month (days_per_month)
		else date_matches_month (days_per_month_leap_year)
	    end
    in
	reasonable_year () andalso reasonable_month () andalso reasonable_day()
    end
