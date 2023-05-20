signature BIGINT = 
sig
type bigint 
exception bigint_error
val get_mag : bigint -> int list
val get_sign : bigint -> bool
val std : bigint -> bigint
val make_bigint : bool * int list -> bigint
val comp : bigint * bigint -> bool
val max : bigint * bigint -> bigint
val equal : bigint * bigint -> bool
val add : bigint * bigint -> bigint
val subtract : bigint * bigint -> bigint
val multiply : bigint * bigint -> bigint
val divide : bigint * bigint -> bigint
val modulo : bigint * bigint -> bigint
val gcd : bigint * bigint -> bigint
val print_bigint : bigint -> string
end;

structure BigInt : BIGINT =
struct
    type bigint = bool * (int list)
    exception bigint_error

    fun get_mag (a : bigint) =    
        #2 a

    fun get_sign (a : bigint) =    
        #1 a

    fun remove_zero_rev(n : int list) = 
        if List.length (n) = 0 then 
            n
        else if hd n = 0 then        
            remove_zero_rev(tl n)
        else
            n

    fun remove_zero (n : int list) = 
        List.rev (remove_zero_rev (List.rev n))

    fun std (num : bigint) = 
        let
        val n = #2 num
        val s = #1 num
        val n_final = remove_zero(n)
        in 
        if List.length n_final = 0 then 
            (false, [])
        else 
            (s,remove_zero(n))
        end

    fun make_bigint (sign : bool, mag : int list) = 
        std (sign, mag)
  
    fun rev_add_intlist (n1 : int list, n2 : int list, partial_sum : int list, carry : int) : int list = 
        if List.length n1 = 0 andalso List.length n2 = 0 andalso carry = 0 then 
            remove_zero_rev(partial_sum)

        else if List.length n1 = 0 andalso List.length n2 = 0 then 
            rev_add_intlist ([carry],[],partial_sum,0)

        else if List.length n1 = 0 then 
            let
            val d1 = 0
            val d2 = hd (n2)
            val curr_sum = d1 + d2 + carry
            in 
            rev_add_intlist(n1, tl n2, curr_sum mod 10 :: partial_sum, curr_sum div 10)
            end

        else if List.length n2 = 0 then 
            let
            val d1 = hd (n1)
            val d2 = 0
            val curr_sum = d1 + d2 + carry
            in 
            rev_add_intlist(tl n1, n2, curr_sum mod 10 :: partial_sum, curr_sum div 10)
            end

        else 
            let
            val d1 = hd (n1)
            val d2 = hd (n2)
            val curr_sum = d1 + d2 + carry
            in 
            rev_add_intlist(tl n1, tl n2, curr_sum mod 10 :: partial_sum, curr_sum div 10)
            end

    fun add_intlist (n1 : int list, n2 : int list) : int list = 
        List.rev (rev_add_intlist(n1, n2, [], 0))

    fun comp_std_rev_intlist (a : int list, b : int list) = 
        (* return true if a < b *)
        if List.length a < List.length b then 
            true
        else if List.length b < List.length a then 
            false
        else if List.length a = 0 andalso List.length b = 0 then 
            false
        else if hd a < hd b then 
            true
        else if hd a > hd b then 
            false 
        else 
            comp_std_rev_intlist(tl a, tl b)

    fun comp (a : bigint, b : bigint) = 
        (* return true if a < b *)
        let
        val a_std = std a
        val b_std = std b
        val s1 = #1 a_std
        val s2 = #1 b_std
        val n1_rev = List.rev (#2 a_std)
        val n2_rev = List.rev (#2 b_std)
        in 
        if s1 andalso (not s2) then 
            true
        else if s2 andalso (not s1) then 
            false
        else if s1 andalso s2 then 
            not (comp_std_rev_intlist (n1_rev,n2_rev))
        else 
            comp_std_rev_intlist (n1_rev,n2_rev)
        end

    fun max (a : bigint, b : bigint) = 
        if comp(a,b) then b else a

    fun equal (a : bigint, b : bigint) =    
        let 
        val a_std = std a 
        val b_std = std b
        in 
        not (comp(a,b) orelse comp(b,a))
        end
      
    fun rev_multiply_digit (n : int list, d : int, partial_prod : int list, carry : int) = 
        if List.length (n) = 0 then
            carry::partial_prod
        else
            let
            val digit = hd (n)
            val curr_prod = digit * d + carry
            in 
            rev_multiply_digit(tl n, d, curr_prod mod 10 :: partial_prod, curr_prod div 10)
            end           

    fun mul_digit (n : int list, d : int) = 
        List.rev (rev_multiply_digit(n,d,[],0))

    fun multiply_intlist (a : int list, b : int list) = 
        if List.length b = 0 then 
            [] 
        else 
            add_intlist(0::multiply_intlist(a,tl b), mul_digit(a,hd b))

    fun multiply (a : bigint, b : bigint) = 
        let
        val s1 = #1 a
        val s2 = #1 b
        val n1 = #2 a
        val n2 = #2 b
        val sign = (s1 orelse s2) andalso not(s1 andalso s2)
        in
        (sign, multiply_intlist(n1,n2))
        end

    fun rev_sub (n1 : int list, n2 : int list, partial_diff : int list, borrow : bool) = 
    (*input specification : n1 > n2 when interpreted as an integer*)
        if List.length n2 = 0 then 
            if borrow = true then 
                rev_sub(n1,[1],partial_diff,false)
            else if List.length n1 = 0 then
                partial_diff
            else
                List.rev(n1)@partial_diff
        else 
            let
            val d1 = hd (n1)
            val d2 = hd (n2)
            val curr_diff = d1 - d2
            in 
            if curr_diff < 0 then
                if borrow = true then
                rev_sub(tl n1, tl n2, curr_diff - 1 + 10 :: partial_diff, true)
                else
                rev_sub(tl n1, tl n2, curr_diff + 10 :: partial_diff, true)
            else
                if borrow = true then 
                    if curr_diff = 0 then 
                        rev_sub(tl n1, tl n2, curr_diff + 10 - 1 :: partial_diff, true)
                    else
                        rev_sub(tl n1, tl n2, curr_diff - 1 :: partial_diff, false)
                else
                    rev_sub(tl n1, tl n2, curr_diff :: partial_diff, false)
            end

    fun sub_intlist (n1 : int list, n2 : int list) = 
        List.rev (rev_sub(n1, n2, [], false))

    fun add (a : bigint, b : bigint) : bigint = 
        let
        val s1 = #1 a
        val s2 = #1 b
        val n1 = #2 a
        val n2 = #2 b
        in
        if (not s1) andalso (not s2) then 
            (false, add_intlist(n1,n2))
        else if (not s1) andalso s2 then 
            if comp ((false,n1),(false,n2)) then
                (true, sub_intlist(n2,n1))
            else 
                (false, sub_intlist(n1,n2))
        else if s1 andalso (not s2) then 
            add (b,a)
        else 
            (true, add_intlist(n1,n2))
        end

    fun subtract (a : bigint, b : bigint) =  
        let
        val s1 = #1 a
        val s2 = #1 b
        val n1 = #2 a
        val n2 = #2 b
        in
        add ((s1,n1),(not s2,n2))
        end
        
    fun div_intlist (a : int list, b : int list, curr_ans : int list, curr_rem : int list) = 
        if List.length a = 0 then 
            curr_ans
        else 
        let
        val new_rem = hd a :: curr_rem
        val new_a = tl a
        in
        if not( comp((false,new_rem),multiply((false,[9]),(false,b)))) then 
         div_intlist(new_a,b,9 :: curr_ans,#2 (subtract((false,new_rem),multiply((false,[9]),(false,b)))))
        else if not( comp((false,new_rem),multiply((false,[8]),(false,b)))) then 
 	 	 div_intlist(new_a,b,8 :: curr_ans,#2 (subtract((false,new_rem),multiply((false,[8]),(false,b)))))
        else if not( comp((false,new_rem),multiply((false,[7]),(false,b)))) then 
 	 	 div_intlist(new_a,b,7 :: curr_ans,#2 (subtract((false,new_rem),multiply((false,[7]),(false,b)))))
        else if not( comp((false,new_rem),multiply((false,[6]),(false,b)))) then 
 	 	 div_intlist(new_a,b,6 :: curr_ans,#2 (subtract((false,new_rem),multiply((false,[6]),(false,b)))))
        else if not( comp((false,new_rem),multiply((false,[5]),(false,b)))) then 
 	 	 div_intlist(new_a,b,5 :: curr_ans,#2 (subtract((false,new_rem),multiply((false,[5]),(false,b)))))
        else if not( comp((false,new_rem),multiply((false,[4]),(false,b)))) then 
 	 	 div_intlist(new_a,b,4 :: curr_ans,#2 (subtract((false,new_rem),multiply((false,[4]),(false,b)))))
        else if not( comp((false,new_rem),multiply((false,[3]),(false,b)))) then 
 	 	 div_intlist(new_a,b,3 :: curr_ans,#2 (subtract((false,new_rem),multiply((false,[3]),(false,b)))))
        else if not( comp((false,new_rem),multiply((false,[2]),(false,b)))) then 
 	 	 div_intlist(new_a,b,2 :: curr_ans,#2 (subtract((false,new_rem),multiply((false,[2]),(false,b)))))
        else if not( comp((false,new_rem),multiply((false,[1]),(false,b)))) then 
 	 	 div_intlist(new_a,b,1 :: curr_ans,#2 (subtract((false,new_rem),multiply((false,[1]),(false,b)))))
        else 
            div_intlist(new_a,b,0 :: curr_ans,new_rem)
        end

    fun div_intlist_final (a : int list, b : int list) =  
        div_intlist(List.rev a,b,[],[])

    fun divide (a : bigint, b : bigint) = 
        let
        val s1 = #1 a
        val s2 = #1 b
        val n1 = #2 a
        val n2 = #2 b
        val sign = (s1 orelse s2) andalso not(s1 andalso s2)     
        in
        std (sign, div_intlist_final(n1,n2))
        end

    fun modulo (a : bigint, b : bigint) = 
        std (subtract (a, multiply (b, divide (a,b))))

    fun gcd (a : bigint, b : bigint) = 
        let
        val x = std a
        val y = std b
        val s1 = #1 x
        val s2 = #1 y
        val n1 = #2 x
        val n2 = #2 y
        in 
        if s1 orelse s2 then 
            gcd ((false,n1),(false,n2))
        else if List.length n1 = 0 then 
            (false, n2)
        else 
            gcd (modulo((false,n2), (false,n1)), (false,n1))
        end 


    fun int_print (a : bigint) = 
        List.rev(map(fn x=>chr(x+48)) (get_mag (a)))
    
    fun print_bigint (a : bigint) = 
        if length (#2 (std a)) = 0 then 
            "0"
        else if get_sign (a) then 
            "~" ^ String.implode (int_print(a))
        else 
            String.implode (int_print(a))

end;


signature RATIONALIZER =
sig
type rational
type bigint
exception rat_error
val make_rat: bigint * bigint -> rational option
val rat: bigint -> rational option
val reci: bigint -> rational option
val neg: rational -> rational
val inverse : rational -> rational option
val equal : rational * rational -> bool (* equality *)
val less : rational * rational -> bool (* less than *)
val add : rational * rational -> rational (* addition *)
val subtract : rational * rational -> rational (* subtraction *)
val multiply : rational * rational -> rational (* multiplication *)
val divide : rational * rational -> rational option (* division *)
val showRat : rational -> string
val showDecimal : rational -> string
val fromDecimal : string -> rational
val toDecimal : rational -> string
end;


functor Rationalizer (BigInt : BIGINT) : RATIONALIZER = 
struct
    type bigint = BigInt.bigint
    type rational = BigInt.bigint * BigInt.bigint
    exception rat_error

    fun make_rat (a : BigInt.bigint, b : BigInt.bigint) = 
        if List.length (BigInt.get_mag (BigInt.std b)) = 0 then 
            NONE
        else 
            let
            val s1 = BigInt.get_sign(a)
            val s2 = BigInt.get_sign(b)
            val n1 = BigInt.get_mag(a)
            val n2 = BigInt.get_mag(b)
            val sign = (s1 orelse s2) andalso not(s1 andalso s2)  
            val gcd = BigInt.gcd(a,b)
            val num = BigInt.divide(BigInt.make_bigint(false,n1),gcd)
            val den = BigInt.divide(BigInt.make_bigint(false,n2),gcd)   
            in
            SOME((BigInt.make_bigint(sign,BigInt.get_mag (num)),den))
            end
    
    fun rat (a : BigInt.bigint) = 
        make_rat(a,BigInt.make_bigint(false,[1]))

    fun reci (a : BigInt.bigint) = 
        if List.length (BigInt.get_mag(BigInt.std a)) = 0 then 
            NONE
        else 
            make_rat(BigInt.make_bigint(false,[1]),a)

    fun neg (a : rational) = 
        let
        val num = #1 a
        val den = #2 a
        val sign = BigInt.get_sign num 
        val num_mag = BigInt.get_mag num
        in
        (BigInt.make_bigint(not sign, num_mag),den)
        end

    fun inverse (a : rational) = 
        make_rat (#2 a, #1 a)
    
    fun equal (a : rational, b : rational) = 
        let 
        val x = valOf (make_rat (#1 a, #2 a))
        val y = valOf (make_rat (#1 b, #2 b))
        val p1 = #1 x
        val q1 = #2 x
        val p2 = #1 y
        val q2 = #2 y
        in
        not (BigInt.comp (p1,p2) orelse BigInt.comp (p2,p1) orelse BigInt.comp (q1,q2) orelse BigInt.comp (q2,q1))
        end 

    fun less (a : rational, b : rational) = 
        let 
        val x = valOf (make_rat (#1 a, #2 a))
        val y = valOf (make_rat (#1 b, #2 b))
        val p1 = #1 x
        val q1 = #2 x
        val p2 = #1 y
        val q2 = #2 y
        in
        BigInt.comp (BigInt.multiply(p1,q2), BigInt.multiply(p2,q1))
        end 

    fun add (a : rational, b : rational) = 
        let 
        val x = valOf (make_rat (#1 a, #2 a))
        val y = valOf (make_rat (#1 b, #2 b))
        val p1 = #1 x
        val q1 = #2 x
        val p2 = #1 y
        val q2 = #2 y
        in
        valOf (make_rat(BigInt.add(BigInt.multiply(p1,q2), BigInt.multiply(p2,q1)), BigInt.multiply(q1,q2)))
        end 

    fun subtract (a : rational, b : rational) = 
        add(a, neg b)

    fun multiply (a : rational, b : rational) =     
        let 
        val x = valOf (make_rat (#1 a, #2 a))
        val y = valOf (make_rat (#1 b, #2 b))
        val p1 = #1 x
        val q1 = #2 x
        val p2 = #1 y
        val q2 = #2 y
        in
        valOf (make_rat( BigInt.multiply(p1,p2), BigInt.multiply(q1,q2)))
        end 

    fun divide (a : rational, b : rational) = 
        let
        val zero = BigInt.make_bigint(false,[])
        val one = BigInt.make_bigint (false,[1])
        in
        if equal(b,valOf(make_rat(zero,one))) then 
            NONE 
        else 
            SOME(multiply(a,valOf(inverse(b)))) 
        end

    fun showRat (a : rational) = 
        let 
        val x = valOf (make_rat (#1 a, #2 a))
        val p = #1 x 
        val q = #2 x
        val zero = BigInt.make_bigint (false,[])

        fun int_print (a : BigInt.bigint) = 
            List.rev(map(fn x=>chr(x+48)) (BigInt.get_mag (a)))
        in 
        if BigInt.equal (p,zero) then 
            String.implode ([#"0",#"/"] @ int_print(q))
        else if BigInt.get_sign (p) = true then     
            String.implode ([#"~"] @ int_print(p) @ [#"/"]@int_print(q))
        else 
            String.implode (int_print(p) @ [#"/"] @ int_print(q))
        end 

    fun sign(a : string) =  
        let
        val s = String.explode a
        in
        if hd s = #"~" then 
            (true, tl s)
        else if hd s = #"+" then 
            (false, tl s)
        else 
            (false, s)
        end

    fun before_point(a : char list, curr_ans : int list) = 
        if hd a = #"." then     
            (tl a, curr_ans)
        else 
            before_point (tl a, ord(hd a) - 48 :: curr_ans)

    fun before_paranthesis(a : char list, curr_ans : int list) = 
        if hd a = #"(" then     
            (tl a, curr_ans)
        else 
            before_paranthesis (tl a, ord(hd a) - 48 :: curr_ans)
    
    fun after_paranthesis(a : char list, curr_ans : int list) = 
        if hd a = #")" then     
            curr_ans
        else 
            after_paranthesis (tl a, ord(hd a) - 48 :: curr_ans)
    
    fun fromDecimal(a : string) = 
        let 
        val (s, p) = sign(a)
        val (q, x) = before_point(p, [])
        val (r, y) = before_paranthesis(q, [])
        val z = after_paranthesis(r, [])
        val b = List.length y
        val c = List.length z
        val xy : BigInt.bigint = BigInt.make_bigint(false, y @ x)
        val xyz : BigInt.bigint = BigInt.make_bigint(false, z @ y @ x)

        fun power_10 (a : int) =   
            if a = 0 then [1] else 0 :: power_10 (a-1)
        
        fun power_10_int (a : int) = 
            BigInt.make_bigint (false, power_10(a))

        in 
        if s = false then 
        valOf (make_rat(BigInt.subtract(xyz,xy),BigInt.subtract(power_10_int(b+c),power_10_int(b))))
        else 
        neg(valOf (make_rat(BigInt.subtract(xyz,xy),BigInt.subtract(power_10_int(b+c),power_10_int(b)))))
        end 

    fun len_recur_helper (n : BigInt.bigint, curr : BigInt.bigint, ans : BigInt.bigint) =     
        let 
        val one = BigInt.make_bigint(false, [1])
        val zero = BigInt.make_bigint(false, [0])
        val nine = BigInt.make_bigint(false, [9])
        val ten = BigInt.make_bigint(false, [0,1])
        val x = BigInt.modulo(curr,n)
        in 
        if BigInt.equal(zero,BigInt.modulo(curr,n)) then 
            ans
        else 
            len_recur_helper(n, BigInt.make_bigint(false,9::(BigInt.get_mag(curr))), BigInt.add(ans,one))
        end

    fun find_len_recur (n : BigInt.bigint) = 
        let
        val nine = BigInt.make_bigint(false, [9])
        val one = BigInt.make_bigint (false,[1])      
        in
        len_recur_helper(n,nine,one)
        end

    fun power_prime(dividend : BigInt.bigint, divisor : BigInt.bigint) = 
    let 
    val zero = BigInt.make_bigint (false, [0])
    val one = BigInt.make_bigint (false, [1])

    in 
    if not (BigInt.equal (zero, BigInt.modulo(dividend,divisor))) then 
        zero
    else 
        BigInt.add (power_prime(BigInt.divide(dividend,divisor),divisor) , one)
    end 

    fun nth_power (base : BigInt.bigint, exponent : BigInt.bigint) = 
    let 
    val zero = BigInt.make_bigint (false, [0])
    val one = BigInt.make_bigint (false, [1])
    in 
        if BigInt.equal(exponent,zero) then 
            one
        else 
            BigInt.multiply (base, nth_power(base, BigInt.subtract(exponent,one)))
    end 

    fun toDecimal_positive (a : rational) = 
        let 
        val int_part = (BigInt.divide (#1 a, #2 a))
        val frac_part = BigInt.modulo (#1 a, #2 a)

        val ten = BigInt.make_bigint (false, [0,1])
        val zero = BigInt.make_bigint (false, [0])

        val rat = valOf (make_rat (frac_part, #2 a))
        val p = #1 rat
        val q = #2 rat

        val two = BigInt.make_bigint(false, [2])
        val five = BigInt.make_bigint(false, [5])

        val pow_2 = power_prime(q,two)
        val pow_5 = power_prime(q,five)

        val rest = BigInt.divide (BigInt.divide (q,nth_power(two,pow_2)), (nth_power(five,pow_5)))
        val len_non_recur = BigInt.max(pow_2,pow_5)

        val len_recur = find_len_recur(rest)

        fun n_9s(n : BigInt.bigint) = 
            let
            val zero = BigInt.make_bigint(false,[0])
            val one = BigInt.make_bigint(false,[1])

            in
            if BigInt.equal (n,zero) then 
                zero
            else 
                BigInt.make_bigint(false,9::BigInt.get_mag(n_9s(BigInt.subtract(n,one))))
            end 

        val nines = n_9s(len_recur) 
        val multiplier = BigInt.divide(nines,rest)
        
        val num_inter = BigInt.multiply(p,nth_power(two,BigInt.max(zero,BigInt.subtract(pow_5,pow_2)))) 
        val num_inter_2 = BigInt.multiply(num_inter,nth_power(five,BigInt.max(zero,BigInt.subtract(pow_2,pow_5))))
        val num = BigInt.multiply(num_inter_2,multiplier)

        val non_repeating = BigInt.divide(num,nines)
        val repeating = BigInt.modulo (num,nines)
        
        in 
        (int_part,non_repeating,repeating,len_recur,len_non_recur)
        end 

    fun get_length (l : int list) = 
        let 
        val zero = BigInt.make_bigint(false,[])
        val one = BigInt.make_bigint(false,[1])
        in 
        if List.length l = 0 then 
            zero 
        else 
            BigInt.add(one,get_length(tl l))
        end 

    fun make_length (l : int list, len : BigInt.bigint) =   
        let 
        val curr_len = get_length l
        in 
        if BigInt.equal(curr_len,len) then 
            l
        else 
            make_length(0::l,len)
        end 

    fun toDecimalCharList (a : rational) = 
        let
        val (int_part,non_repeating,repeating,len_recur,len_non_recur) = toDecimal_positive (a)
        val non_repeating_list = List.rev(BigInt.get_mag non_repeating)
        val non_repeating = make_length (non_repeating_list, len_non_recur)
        val repeating_list = List.rev(BigInt.get_mag repeating)
        val repeating = make_length (repeating_list, len_recur)
        fun int_print (a : int list) = 
            map(fn x=>chr(x+48)) (a)
        val int_part_char_list = List.rev(int_print(BigInt.get_mag int_part))
        in 
        if BigInt.get_sign(#1 a) then 
            #"~" :: toDecimalCharList (neg a)
        else
            int_part_char_list @ [#"."] @ int_print (non_repeating) @ [#"("] @ int_print(repeating) @ [#")"]
        end 

    fun toDecimal (a : rational) = String.implode (toDecimalCharList a)

    fun showDecimal (a : rational) = toDecimal (a)
end;

structure Rational = Rationalizer(BigInt)