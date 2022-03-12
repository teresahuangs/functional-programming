(* A.1 *)
(*
1. - : int = 10
2. - float = 10.
3. - : int = 12
4. Error: This expression has type float but an expression was expected of type
         int
    This error happened because + is for integer addition and we inputted 2 floats
5. Error: This expression has type int but an expression was expected of type
         float
    This error happened because +. is for float addition and we inputted 2 integers
6. Error: This expression has type float but an expression was expected of type
         int
    This error happened because + is for integer addition and we inputted 1 float
    and one integer
7. Error: This expression has type int but an expression was expected of type
         float
    This error happened because +. is for float addition and we inputted 1 float
    and one integer
8. - : float = 7.2
9. - : int = 5
10. - : int = 7
11. val a : int = 3
12. val b : int = 4
13. - : bool = false
14. - : bool = true
15. - : bool = false
    It is different because == is looking for the expressions to be identical
    and = is looking for the expression to be structurally the same
16. - : (int * int * int) list = [(1, 2, 3)]
17. - : (int * int * int) list = [(1, 2, 3)]
    This does this because commas mean that the elements are elements of a tuple,
    so to differentiate items, we should use semicolons
18. - : int = 4
19. Error: Syntax error
    The error happened because the syntax for and is && in OCaml
20. - : int = 6
21. - : int = 4
    It is different because the 2 is only to a if b is not greater than a,
    whereas it is added to any result of the expression in the previous expression
22. - : int = 6
23. Error: This expression has type int but an expression was expected of type
         unit because it is in the result of a conditional with no else branch
        This type error is happening because OCaml is assuming type unit but since 
        the then is int then the else should also be int, but because there is no else,
        we have a type error when the interpreter assumes a type when it should be int.
*)

(* A.2 *)

let sum_of_squares_of_two_largest a b c =
    if a > b && b > c
        then a * a + b * b
        else if c > b
            then c * c + a * a 
            else a * a + c * c

(* A.3 *)
(*
The following function will first evaluate if b is greater than 0 and if it is then it will do the 
function in which it adds a and b and if b is less than 0, we can see that the expression would be
a - (-b) which is essentially a + b. So this function essentially gives a + the absolute value of b.
*)

(* B.1 *)
(*
The behavior that Ben will observe with an interpreter that uses applicative order evaluation is
that the interpreter will first evaluate p() which will not return a value because it will
be stuck in an infinite loop because p is recursive so it continuously calls itself with no end.
The behavior that Ben will observe with an interpreter that uses normal order evaluation is that
the interpreter will return 0 because p() is not evaluated.
*)

(* B.2 *)
(*
When Alyssa tries to use this to compute sqaure roots, our sqrt_iter function will be in an infinite loop
because the predicate is not evaluated before the arguments of new_if are, therefore the else will be
recursively called continuously forever because it will always be evaluated regardless of the result of
the predicate.

*)

(* B.3.1 *)
(*
  add_a is linear recursive, while add_b is linear iterative because of tail recursion optimization.
 *)

(* B.3.2 *)
(*
let rec add_a a b =
  if a = 0
    then b
    else inc (add_a (dec a) b)

Desugar this to:

let 

let rec add_a =
    fun a b -> 
    if a = 0
        then b
        else inc (add_a (dec a) b)

Evaluate (add_a 2 5)
    evaluate 2 -> 2
    evaluate 5 -> 5
    evaulate add_a -> fun a b -> ...
    apply (fun a b -> if ...) to 2, 5
    substitute 2 for a, 5 for b in (if ...)
        if 2 = 0 then 5 else inc (add_a (dec 2) 5)
    evaluate if 2 = 0 then 5 else inc (add_a (dec 2) 5)
            if is a special form, so evaluate the first operand:
        evaluate (2 = 0)
            evaluate 2 -> 2
            evaluate 0 -> 0 
            evaluate = -> [primitive function =]
            apply = to 2, 0 -> false
        first argument of if is false, so evaluate the third operand:
        evaluate inc (add_a (dec 2) 5)
            evaluate (dec 2)
                evaluate dec -> [primitive function]
                apply dec to 2 -> 1
            evaluate 5 -> 5
            evaluate add_a -> fun a b -> ...
            apply (fun a b -> if ...) to 1, 5
                substitute 1 for a, 5 for b in (if ...)
                    if 1 = 0 then 5 else inc (add_a (dec 1) 5)
                evaluate if 1 = 0 then 5 else inc (add_a (dec 1) 5)
                    if is a special form, so evaluate the first operand:
                    evaluate 1 = 0
                    evaluate 2 -> 2
                    evaluate 0 -> 0
                    evaluate = -> [primitive function =]
                    apply = to 2, 0 -> false
                first argument of if is false, so evaluate the third operand:
                evaluate inc (add_a (dec 1) 5)
                    evaluate dec 1
                        evaluate 1 -> 1
                        evaluate dec -> [primitive function]
                        apply dec to 1 -> 0
                    evaluate 5 -> 5
                    evaluate add_a -> fun a b -> ...
                    apply fun a b -> ... to 0, 5
                        substitute 0 for a, 5 for b in (if ...)
                            if 0 = 0 then 5 else inc (add_a (dec 0) 5)
                        evaluate if 0 = 0 then 5 else inc (add_a (dec 0) 5)
                            if is a special form, so evaluate the first operand:
                            evaluate 0 = 0
                                evaluate 0 -> 0
                                evaluate 0 -> 0 
                                evaluate = -> 0
                                apply = to 0, 0 -> true
                            first argument of if is true, so evaluate the second operand:
                            evaluate 5 -> 5
                        evaluate inc -> fun a -> a +1
                        apply fun a to 5 -> 6
                        evaluate inc -> fun a -> a + 1
                        apply fun a to 6 -> 7
                            result : 7



 *)

(*
let rec add_b a b =
  if a = 0
     then b
     else add_b (dec a) (inc b)

Desugar this to:

let rec add_b =
  fun a b ->
    if a = 0
       then b
       else add_b (dec a) (inc b)

Bind the name "add_b" to the value:

  fun a b ->
    if a = 0
       then b
       else add_b (dec a) (inc b)

Evaluate (add_b 2 5)
>>> evaluate 2 -> 2
>>> evaluate 5 -> 5
>>> evaluate add_b -> fun a b -> ... 
  apply (fun a b -> if ...) to 2, 5
  substitute 2 for a, 5 for b in (if ...)
    -> if 2 = 0 then 5 else add_b (dec 2) (inc 5)
  evaluate (if 2 = 0 then 5 else add_b (dec 2) (inc 5))
    if is a special form, so evaluate the first operand:
      evaluate (2 = 0)
        >>> evaluate 2 -> 2
        >>> evaluate 0 -> 0 
        >>> evaluate = -> [primitive function =]
        apply = to 2, 0 -> false
    first argument of if is false, so evaluate the third operand:
      evaluate (add_b (dec 2) (inc 5))
        evaluate (dec 2)
          >>> evaluate 2 -> 2
          >>> evaluate dec -> [primitive function]
          apply dec to 2 -> 1
        evaluate (inc 5)
        >>> evaluate 5 -> 5
        >>> evaluate inc -> [primitive function]
          apply inc to 5 -> 6
        >>> evaluate add_b -> fun a b -> ...
        apply (fun a b -> if ...) to 1, 6
        substitute 1 for a, 6 for b in (if ...)
          -> if 1 = 0 then 6 else add_b (dec 1) (inc 6)
        evaluate (if 1 = 0 then 6 else add_b (dec 1) (inc 6))
          if is a special form, so evaluate the first operand:
            evaluate (1 = 0)
            >>> evaluate 1 -> 1
            >>> evaluate 0 -> 0
            >>> evaluate = -> [primitive function =]
              apply = to 1, 0 -> false
          first argument of if is false, so evaluate the third operand:
            evaluate (add_b (dec 1) (inc 6))
              evaluate (dec 1)
              >>> evaluate 1 -> 1
              >>> evaluate dec -> [primitive function]
                apply dec to 1 -> 0
              evaluate (inc 6)
                >>> evaluate 6 -> 6
                >>> evaluate inc -> [primitive function]
                apply inc to 6 -> 7
                >>> evaluate add_b -> fun a b -> ...
              apply (fun a b -> if ...) to 0, 7
              substitute 0 for a, 7 for b in (if ...)
                -> if 0 = 0 then 7 else add_b (dec 0) (inc 7)
              evaluate (if 0 = 0 then 7 else add_b (dec 0) (inc 7))
                if is a special form, so evaluate the first operand:
                  evaluate (0 = 0)
                  >>> evaluate 0 -> 0
                  >>> evaluate 0 -> 0
                  >>> evaluate = -> [primitive function =]
                    apply = to 0, 0 -> true
                first argument of if is true, so evaluate the second operand:
                evaluate 7 -> 7
                  result: 7
*)

(* C.1 *)

(* This function computes the factorial of the input number,
   which for a number n is equal to n * (n-1) * ... * 1. *)

let rec factorial n =
  if n = 0 then 1 else n * factorial (n - 1)


(* C.1.a *)
let e_term num =
    1.0 /. float_of_int (factorial num) 

(* C.1.b *)
let rec e_approximation num =
    if num = 0 then e_term 0 else e_term num +. e_approximation(num - 1)

(* C.1.c *)
(*
    exp 1.0 = 2.71828182845904553
    e_approximation 20 = 2.71828182845904509
 *)

(* C.1.d *)
(*
    If we tried to compute a better approximation to e, it will default to 0 because
    we do not have the storage for it and because 1 over infity is infinity the sum of
    the e_terms will be infinity.
 *)

(* C.2 *)
let rec is_even num = match num with
  | 0 -> true
  | _ -> is_odd(num-1)
and is_odd num = match num with
  | 0 -> false
  | _ -> is_even(num-1)

(* C.3 *)
let rec f_rec num =
    if num < 3 then num else f_rec (num - 1) + 2 * f_rec(num - 2) + 3 * f_rec(num - 3)

let rec helper num1 num2 num3 count max =
    if count = max 
    then num3 + 2 * num2 + 3 * num1
    else helper (num2) (num3) (num3 + 2 * num2 + 3 * num1) (count+1) max

let f_iter num =
    if num < 3 then num
    else helper 0 1 2 3 num


(* C.4 *)
let rec pascal_coefficient a b =
    match (a, b) with
        | (a', b') when a' < 1 || b' < 1 -> failwith "invalid arguments"
        | (a', b') when b' > a' -> failwith "invalid arguments"
        | (a', b') when b' = a' -> 1
        | (_, 1) -> 1
        | (_, _) -> pascal_coefficient (a - 1) (b - 1) + pascal_coefficient (a - 1) b


