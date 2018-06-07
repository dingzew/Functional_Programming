fun is_older (day1 : (int * int * int), day2 : (int * int * int)) =
    if #1 day1 < #1 day2
    then true
    else
	if #1 day1 > #1 day2
	then false
	else	    
            if #2 day1 < #2 day2			    
	    then true		     
	    else
		if #2 day1 > #2 day2
		then false
		else		    
		    if #3 day1 < #3 day2				    
		    then true			     
		    else false
			     
	
fun number_in_month (date_list : (int * int * int) list, month : int) =
    if null date_list
    then 0
    else
	if #2 (hd date_list) = month
	then number_in_month(tl date_list, month) + 1
	else number_in_month(tl date_list, month)

fun number_in_months (date_list : (int * int * int) list, month_list : int list) =
    if null month_list
    then 0
    else number_in_month(date_list, hd month_list) + number_in_months(date_list, tl month_list)

fun dates_in_month (date_list : (int * int * int) list, month : int) =
    if null date_list
    then []
    else
	if #2 (hd date_list) = month		     
	then hd date_list :: dates_in_month(tl date_list, month)
	else dates_in_month(tl date_list, month)

fun dates_in_months (date_list : (int * int * int) list, month_list : int list) =
    if null month_list
    then []
    else dates_in_month(date_list, hd month_list) @  dates_in_months(date_list, tl month_list)					    
	     
fun get_nth (string_list : string list, n : int) =
    if n = 1
    then hd string_list
    else get_nth(tl string_list, n - 1)

fun date_to_string (date : (int * int * int)) =
    let
	val month_name = ["January", "February", "March", "April",
		      "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(month_name, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date) 
    end

fun number_before_reaching_sum (sum : int, list : int list) =
    if null list
    then 0
    else   
	if hd list >= sum				
	then 0	 
	else 1 + number_before_reaching_sum(sum - hd list,tl list)
		 

	    
fun what_month (day : int) =
    let
	val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum(day, days_in_month) + 1
    end

fun month_range (day1 : int, day2 : int) =
    if day2 < day1
    then []
    else	
	if day1 = day2		      
	then [what_month(day2)]		 
	else what_month(day1) :: month_range(day1 + 1, day2)
					    

fun oldest (date_list : (int * int * int) list) =
   if null date_list
    then NONE
    else
	let
	    fun oldest_noempty (date_list : (int * int * int) list) =
		if null (tl date_list)
		then hd date_list			
		else let val next_oldest = oldest_noempty(tl date_list)
		     in
			 if is_older(hd date_list, next_oldest)
			 then hd date_list
			 else next_oldest
		     end			 	    
	in
	    SOME(oldest_noempty(date_list))
	end

	    
fun number_in_months_challenge (date_list : (int * int * int) list, month_list : int list) =
    let	
	fun remove_du (list : int list) =
	    if null list
	    then []
	    else		
		let		    
		    fun check_n (list : int list, n : int) =			
			if null list				      
			then []				 
			else			    
			    let val tail = check_n(tl list, n)					      
			    in				
				if hd list = n						 
				then tail					 
				else hd list :: tail						    
			    end
		in
		    hd list :: check_n(remove_du(tl list), hd list)
		end
	val good_month_list = remove_du(month_list)				       
    in	
	number_in_months(date_list, good_month_list)				       
    end
	
	    
	    
fun dates_in_months_challenge (date_list : (int * int * int) list, month_list : int list) =
    let	
	fun remove_du (list : int list) =
	    if null list
	    then []
	    else		
		let		    
		    fun check_n (list : int list, n : int) =			
			if null list				      
			then []				 
			else			    
			    let val tail = check_n(tl list, n)					      
			    in				
				if hd list = n						 
				then tail					 
				else hd list :: tail						    
			    end
		in
		    hd list :: check_n(remove_du(tl list), hd list)
		end
	val good_month_list = remove_du(month_list)				       
    in	
	dates_in_months(date_list, good_month_list)				       
    end

fun reasonable_date (date : (int * int * int)) =
    if #1 date <= 0 orelse #2 date <= 0 orelse #2 date > 12
    then false
    else
	let
	    val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	    val mon = #2 date
	    val day = #3 date			 
	    fun check_leap (year : int) =
		 if year - (year div 100) * 100 = 0
		 then
		     if year - (year div 400) * 400 = 0
		     then true
		     else false
		 else
		     if year - (year div 4) * 4 = 0
		     then true
		     else false
	    fun get_nth (int_list : int list, n : int) =
		if n = 1			   
		then hd int_list			
		else get_nth(tl int_list, n - 1)			    
		
	in
	    if mon = 2 andalso check_leap(#1 date)
	    then
		if day <= 0 orelse day > 29
		then false
		else true
	    else
		if day <=0 orelse day > get_nth(days_in_month, mon)
		then false
		else true
	end
	    
	    