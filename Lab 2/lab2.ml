open Num
let ni = num_of_int     (* convert int -> num *)

(* A.1 *)
(*
    The space complexity O(n). This is different than time complexity because
    OCaml will evaluate each operand before actually using the operator
    our results from g^n operatiosn do not have to be simultaneously stored. 

 *)

 (* A.2 *)
    (* 
    1. 5 times (12.15 / 3^5 < 0.1)
    2. The order of growth in space is O(log(a)) and the number of steps 
    is also O(log(a)) because we are attempting to evaluate the number of times
    we can reduce 3 until it is smaller than 0.01, thus we know this to be
    log(a). It is the same for steps because the function is called log(a) times.

    *)


 (* A.3.a *)
let rec fast_expt b n =
    let is_even m = m mod 2 = 0 in
    let square m = m * m in
    match n with 
        | 0 -> 1
        | n' when is_even n' -> square (fast_expt b (n / 2))
        | _ -> b * fast_expt b (n - 1)

(* A.3.b *)
let ifast_expt b n =
    let is_even m = m mod 2 = 0 in
    let square m = m * m in
    let rec iter b n a = 
        match n with
        | 0 -> a
        | n' when is_even n' -> iter (square b) (n' / 2) a
        | _ -> iter b (n - 1) (a * b)
    in
        iter b n 1

(* A.4 *)
let rec fast_mult a b =
	let is_even m = m mod 2 = 0 in
	let double m = m * 2 in
	let halve m = m / 2 in
		match b with
			| 0 -> 0
			| b' when is_even b' -> double (fast_mult a (halve b'))
			| _ -> a + fast_mult a (b - 1)

(* A.5 *)
let ifast_mult a b = 
    let is_even m = m mod 2 = 0 in
    let double m = m * 2 in 
    let halve m = m / 2 in 
    let rec iter a b sum = 
        match b with 
            | 0 -> sum
            | b' when is_even b' -> iter (double a) (halve b') sum
            | _ -> iter a (b - 1) (sum + a)
    in
        iter a b 0 

(* A.6 *)
(*
    The worst case space complexity is O(log(n)) and time complexity is
    O(n) because our function is a tree recursive function with log(n) 
    depth. So we know that the depth represents space complexity and
    thus we know that the nodes will be time complexity, which is also
    the number of function calls. So if we know our depth is log(n) because
    there are log(n) pending operations, then we know the number of nodes
    will be 2^(log_2(n)) = n.

 *)

 (* A.7 *)
 (*
    1. The process this function represents is linear recursive because 
    each recursive call makes one more recursive call so that it is constant.
    2. The space and time complexity are both O(n) because there will be n
    pending operations for space complexity and n function calls.
  *)

(* B.1 *)
(*
    a. 
    let x = 20
    and y = 2 * 4
    in x * (2 + y)

    Ans: (fun x y -> x * (2 + y)) 20 (2 * 4)

    b.
    let a = 1.0
    and b = 20.0
    and c = 3.0
    in sqrt (b *. b -. 4.0 *. a *. c)

    Ans: (fun a b c -> sqrt (b *. b -. 4.0 *. a *. c)) 1.0 20.0 3.0
 
    c.
    let x = 1 in
    let y = 2 in
    let z = 3 in
    x * y * z 

    Ans: (fun x y z -> x + y + z) 10 (x * 2) (y + 3)
    
    d. 
    let x = 1 in
    let x = 2 in
    let x = 3 in
    x * x * x

    Ans: (fun x -> (fun x -> (fun x -> x * x * x) 3) 2) 1
    *)


(* B.2 *)
(*
    let x = 2 * 10
    and y = 3 + 4
    in
    let y = 14 in
    let z = 22 in
        x * y * z

evaluate fun x y -> ... 
    evaluate 2 * 10 -> 20
    evaluate 3 + 4 -> 7
    apply (fun x y -> ...) to 20, 7
    substitute 20 for x, 7 for y (y is shielded)
        -> (fun y -> (fun z -> 20 * y * z) 22) 14 
        evaluate (fun y -> (fun z -> 20 * y * z) 22) 14 
            apply (fun y -> ...) to 14
                substitute 14 for y
                    -> (fun z -> 20 * 14 * z) 22
                    evaluate (fun z -> 20 * 14 * z) 22
                        apply (fun z -> ...) to 22
                        substitute 22 for z
                            -> 20 * 14 * 22
                            -> 6160


 *)

 (* B.3 *)
 (*
    (fun x y z -> x + y + z) 10 (x * 2) (y + 3)

    This won't work because we can see that the operands are evaluated first,
    so we see that x, y, and z will be bounded to the operands afterwards,
    thus 10 has not been bounded to x yet. So to fix this we could do
    let x = 10 in
    let y = y * 10 in
    let z = y + 3 in
        x + y + z
  *)


(* C.1 *)
let isum term a next b = 
	let rec iter a result =
		if a >/ b then result else iter (next a) (result +/ (term a))
	in
		iter a (ni 0)

(* C.2 *)
let rec product_rec term a next b =
	if a >/ b then (ni 1) else term a */ (product_rec term (next a) next b)

let product_iter term a next b =
    let rec iter a result =
		if a >/ b then result else iter (next a) (result */ (term a))
	in
		iter a (ni 1)

let factorial_rec n = 
    product_rec (fun x->x) (ni 1) (fun n -> n +/ (ni 1)) n

let factorial_iter n =
    product_iter (fun x->x) (ni 1) (fun n -> n +/ (ni 1)) n

let pi_product n =
	let numerator x = x +/ (ni 2) -/ (mod_num x (ni 2)) in
	let denominator x = x +/ (ni 2) -/ (mod_num (x +/ (ni 1)) (ni 2)) in
	let next x = x +/ (ni 1) in
	let num = product_rec numerator (ni 1) next n in
	let den = product_rec denominator (ni 1) next n in
		(ni 4) */ num // den

let pi_approx = float_of_num(pi_product(ni 3000))


(* C.3 *)
let rec accumulate_rec combiner null_value term a next b =
    if a >/ b then null_value
		else combiner (term a) 
			(accumulate_rec combiner null_value term (next a) next b)


let accumulate_iter combiner null_value term a next b =
    let rec iter a result =
            if a >/ b then result else iter (next a) (combiner result (term a))
        in
            iter a null_value

let sum term a next b =
	accumulate_iter (+/) (ni 0) term a next b

let product term a next b =
	accumulate_iter ( */ ) (ni 1) term a next b


(* C.4 *)
let compose f g =
    fun x -> f(g x)

(* C.5 *)
let rec repeated f n =
	if n = 0 then fun x -> x else compose f (repeated f (n - 1))

(* C.6 *)
let smooth dx f = fun x -> (f (x -. dx) +. f x +. f (x +. dx)) /. 3.0

let nsmoothed dx f n =
    (repeated (smooth dx) n) f

(* D.1 *)
let is_prime n =
    let sqrt_n = int_of_float (sqrt (float_of_int n)) in
        let rec iter i = 
            if i > sqrt_n 
                then true
                else if n mod i = 0 then false else iter (i + 1)
        in
            if n < 2 then false else iter 2

(* D.2 *)
let smallest_prime_factor n =
	let rec iter i =
		if n mod i = 0 && is_prime i then i else iter (i + 1)
	in
		if is_prime n || n < 2 
            then invalid_arg 
				"Invalid argument: input number is prime or less than 2"
			else iter 2
