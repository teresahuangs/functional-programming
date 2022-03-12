(* A.1 *)
type point = { x : float; y : float }
type segment = { startp : point; endp : point }

let midpoint_segment { startp; endp } = 
	let x = (startp.x +. endp.x) /. 2. in
	let y = (startp.y +. endp.y) /. 2. in
		{ x; y }

let segment_length { startp; endp } =
	let lenx = abs_float (endp.x -. startp.x) in
	let leny = abs_float (endp.y -. startp.y) in
		sqrt (lenx *. lenx +. leny *. leny)

let print_point { x; y} =
	Printf.printf "(%g, %g)" x y

let make_point x y = { x; y }
let get_coords { x; y } = (x, y)

let make_segment startp endp = { startp; endp }
let get_points { startp; endp } = (startp, endp)


(* A.2 *)
type rectangle = { ll : point; ur : point }

let rectangle_lower_segment { ll; ur } =
	let startp = ll in
	let endp = { x = ur.x; y = ll.y } in
		{ startp; endp }

let rectangle_upper_segment { ll; ur } =
	let startp = { x = ll.x; y = ur.y } in
	let endp = ur in
		{ startp; endp }
		
let rectangle_left_segment { ll; ur } = 
	let startp = { x = ll.x; y = ur.y } in
	let endp = ll in
		{ startp; endp }
		
let rectangle_right_segment { ll; ur } =
	let startp = ur in
	let endp = { x = ur.x; y = ll.y } in
		{ startp; endp }

type rectangle2 = { lx : float; ux : float; ly : float; uy : float }

let rectangle_lower_segment2 { lx; ux; ly; uy } = 
	let startp = { x = lx; y = ly } in
	let endp = { x = ux; y = ly } in
		{ startp; endp }
		
let rectangle_upper_segment2 { lx; ux; ly; uy } =
	let startp = { x = lx; y = uy } in
	let endp = { x = ux; y = uy } in
		{ startp; endp }
		
let rectangle_left_segment2 { lx; ux; ly; uy } =
	let startp = { x = lx; y = uy } in
	let endp = { x = lx; y = ly } in
		{ startp; endp }
		
let rectangle_right_segment2 { lx; ux; ly; uy } =
	let startp = { x = ux; y = uy } in
	let endp = { x = ux; y = ly } in
		{ startp; endp }
		
let rectangle_perimeter r =
	(segment_length (rectangle_lower_segment r))
		+. (segment_length (rectangle_upper_segment r))
		+. (segment_length (rectangle_left_segment r))
		+. (segment_length (rectangle_right_segment r))

let rectangle_perimeter2 r =
	(segment_length (rectangle_lower_segment2 r))
		+. (segment_length (rectangle_upper_segment2 r))
		+. (segment_length (rectangle_left_segment2 r))
		+. (segment_length (rectangle_right_segment2 r))

let rectangle_area r =
	(segment_length (rectangle_lower_segment r))
		*. (segment_length (rectangle_left_segment r))
		
let rectangle_area2 r =
	(segment_length (rectangle_lower_segment2 r))
		*. (segment_length (rectangle_left_segment2 r))

let make_rectangle ll ur = { ll; ur}

let make_rectangle2 lx ly ux uy = { lx; ly; ux; uy }

(* A.3 *)
let make_pair x y = fun m -> m x y
(* Or, equivalently: let make_pair x y m = m x y *)
let first z = z (fun x y -> x)
let second z = z (fun x y -> y)

(*
    (make_pair x y) will give x for any x and y, because we are  
    passing in (fun m -> m x y) as the argument z into first, which gives us 
    (fun m -> m x y) (fun x y -> x). Next, (fun x y -> x) is being passed in as
    the argument for m, which gives us (fun x y -> x) x y, where x and y are being
    passed in as arguments, thus x is outputted.
 *)

(*
    evaluate second (make_pair 1 2)
        evaluate make_pair 1 2
            evaluate 1 -> 1
            evaluate 2 -> 2
            evaluate make_pair -> fun x y -> fun m -> m x y
            apply fun x y -> ... to 1, 2
                substitute 1 for x, 2 for y -> fun m -> m 1 2
        evaluate second -> fun z -> z (fun x y -> y)
        apply fun z ->... to (fun m -> m 1 2)
            substitute (fun m -> m 1 2) for z -> (fun m - > m 1 2) (fun x y -> y)
            evaluate:
                evaluate (fun x y -> y) -> (fun x y -> y)
                evaluate (fun m -> m 1 2) -> (fun m -> m 1 2)
                apply (fun m -> m 1 2) to (fun x y -> y)
                    substitute (fun x y -> y) for m -> (fun x y -> y) 1 2
                    evaluate:
                        evaluate 1 -> 1
                        evaluate 2 -> 2
                        evaluate (fun x y -> y) -> (fun x y -> y)
                        apply (fun x y -> y) to 1, 2
                        substitute 1 for x, for y -> 2

Answer: 2
 *)

(* A.4 *)
let pow x y =
    if y = 0 then 1 else
    let rec iter ans x y =
      if y = 1 then ans else iter (ans * x) x (y-1) in
    iter x x y

let int_log x y =
    if y mod x <> 0 then 0
    else let rec iter num x y =
           if y mod x <> 0 || y = 1 then num
           else iter (num + 1) x (y/x) in
      iter 0 x y

let make_pairi a b = (pow 2 a) * (pow 3 b)
let firsti p = int_log 2 p
let secondi p = int_log 3 p


(* A.5 *)
let zero = []

let is_zero = function
    | [] -> true
    | () :: _ -> false

let succ u = () :: u

let prev u = match u with
    | [] -> invalid_arg "No unary integer less than zero"
    | ():: after -> after

let integer_to_unary num =
    let rec iter ans num =
        if num = 0 then ans
        else iter (succ ans) (num-1)
    in iter zero num

let unary_to_integer u =
    let rec iter u num =
        if u = zero then num
        else iter (prev u) (num+1) in
    iter u 0

let unary_add a b =
    let rec iter ans b =
        if b = zero then ans
        else iter (succ ans) (prev b) in
    iter a b

type nat = Zero | Succ of nat

let zero' = Zero

let is_zero' = function
    | Zero -> true
    | Succ _ -> false

let succ' u = Succ u

let prev' = function
    | Zero -> invalid_arg "No unary integer less than zero"
    | Succ after -> after

let unary_to_integer' u =
    let rec iter' u num =
        if is_zero' u then num
        else iter' (prev' u) (num+1) in
    iter' u 0

let integer_to_unary' num =
    let rec iter' ans num =
        if num = 0 then ans
        else iter' (succ' ans) (num-1)
    in iter' zero' num

let unary_add' a b =
    let rec iter' ans b =
        if is_zero' b then ans
        else iter' (succ' ans) (prev' b) in
    iter' a b

(* No, the other definitions do not have to change their definitions.  *)

(* A.6 *)
let zerof = fun s -> fun z -> z
  (* or equivalently: let zerof = fun s z -> z *)
  (* or equivalently: let zerof s z = z *)

let add1 n = fun s -> fun z -> s (n s z)
  (* or equivalently: let add1 n = fun s z -> s (n s z) *)
  (* or equivalently: let add1 n s z = s (n s z) *)

let one = fun s -> fun z -> s z
let two = fun s -> fun z -> s (s z)
let three = fun s -> fun z -> s (s (s z))
let four = fun s -> fun z -> s (s (s (s z)))
let five = fun s -> fun z -> s (s (s (s (s z))))
let six = fun s -> fun z -> s (s (s (s (s (s z)))))
let seven = fun s -> fun z -> s (s (s (s (s (s (s z))))))
let eight = fun s -> fun z -> s (s (s (s (s (s (s (s z)))))))
let nine = fun s -> fun z -> s (s (s (s (s (s (s (s (s z))))))))
let ten = fun s -> fun z -> s (s (s (s (s (s (s (s (s (s z)))))))))

let add m n s z = m s (n s z)
let church_to_integer n = n (fun i -> 1 + i) 0

(* A.7 *)
(*
    Both church_to_integer zerof and church_to_integer one 
    return an integer because zerof is 'a -> 'b -> 'b
    and if 'a and 'b are both int then it will return an int.
    For one it is ('a -> 'b) -> 'a -> 'b which is int -> int ->
    int  so if 'a -> 'b is int -> int then the resulting value is int.

 *)

(* B.1 *)
type mobile = Mobile of branch * branch  (* left and right branches *)
and branch =
    | Weight    of int * int     (* length and weight *)
    | Structure of int * mobile  (* length and sub-mobile *)

let make_mobile l r = Mobile (l, r)
let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m)

(* B.1.a *)
let left_branch (Mobile (l, _)) = l

let right_branch (Mobile (_, r)) = r

let branch_length = function
	| Weight (l, _) -> l
	| Structure (l, _) -> l

let branch_structure = function
	| Weight (_, w) -> `Weight w
	| Structure (_, m) -> `Structure m

(* B.1.b *)
let rec branch_weight1 = function
	| Weight (_, w) -> w
	| Structure (_, m) -> total_weight1 m
and total_weight1 (Mobile (l, r)) =
	(branch_weight1 l) + (branch_weight1 r)

let rec branch_weight2 b =
	match branch_structure b with
		| `Weight w -> w
		| `Structure m -> total_weight2 m
and total_weight2 m =
	(branch_weight2 (left_branch m)) + (branch_weight2 (right_branch m))

(* B.1.c *)
let rec is_balanced m =
    let test = (branch_length (left_branch m)) * (branch_weight2 (left_branch m))
                    = (branch_length (right_branch m)) *
                    (branch_weight2 (right_branch m)) in
    match (branch_structure (left_branch m), branch_structure (right_branch m))
    with
    | (`Weight w, `Weight w1) -> test
    | (`Weight w, `Structure s)
    | (`Structure s, `Weight w) -> test && is_balanced s
    | (`Structure s, `Structure s1) -> test && is_balanced s && is_balanced s1

(* B.1.d *)
type mobile'  = { left: branch'; right: branch' }
and  branch'  = Branch' of int * contents
and  contents = Weight' of int | Structure' of mobile'

let make_mobile' left right = { left; right }
let make_weight' l w = Branch' (l, (Weight' w))
let make_structure' l m = Branch' (l, (Structure' m))

let left_branch' { left } = left
let right_branch' { right } = right

let branch_length' (Branch' (l, _)) = l

let branch_structure' (Branch' (_, c)) =
	match c with 
		| Weight' w -> `Weight w
		| Structure' m -> `Structure m

let rec branch_weight' b =
	match branch_structure' b with
		| `Weight w -> w
		| `Structure m -> total_weight' m
and total_weight' m =
	(branch_weight' (left_branch' m)) 
		+ (branch_weight' (right_branch' m))
		
let rec is_balanced' m =
	let is_balanced_branch b =
		match branch_structure' b with
			| `Weight w -> true
			| `Structure m -> is_balanced' m
	in
	let l = left_branch' m in
	let r = right_branch' m in
		if (branch_length' l) * (branch_weight' l) <>
			(branch_length' r) * (branch_weight' r)
			then false
			else (is_balanced_branch l) && (is_balanced_branch r)

(* B.2 *)
type tree = Tree of elem list
and elem =
    | Num of int
    | Sub of tree

let rec square_tree tree1 =
    match tree1 with
    | Tree []-> Tree []
    | Tree((Num h) :: t) -> let (Tree tempTree) = square_tree (Tree t) in
        Tree(Num (h * h) :: tempTree)
    | Tree ((Sub h) :: t) -> let (Tree tempTree) = square_tree (Tree t) in
        Tree(Sub (square_tree h) :: tempTree)

let rec square_tree' treeLst = match treeLst with
    | Tree treeLst ->
        let mappingF = function
            | Num n -> Num (n*n)
            | Sub s -> Sub(square_tree' s)
        in
        Tree (List.map mappingF treeLst)

(* B.3 *)
let rec tree_map f (Tree treeLst) =
	let f_elem = function
		| Num i -> Num (f i)
		| Sub tr -> Sub (tree_map f tr)
	in Tree (List.map f_elem treeLst)


let square_tree'' tree = tree_map (fun n -> n * n) tree


(* C.1 *)
type expr =
    | Int of int           (* constant *)
    | Var of string        (* variable *)
    | Add of expr * expr   (* expr1 + expr2 *)
    | Mul of expr * expr   (* expr1 * expr2 *)
    | Pow of expr * int    (* expr^n *)
      

let rec simplify1 = function
    | Add (Int a, Int b) -> Int (a + b)
    | Mul (Int a, Int b) -> Int (a * b)
    | Pow (Int a, b) -> Int (pow a b)
    | Add (Int 0, expr)
    | Add (expr, Int 0) -> expr
    | Mul (Int 0, expr)
    | Mul (expr, Int 0) -> Int 0
    | Mul (Int 1, expr)
    | Mul (expr, Int 1) -> expr
    | Pow (_, 0) -> Int 1
    | Pow (expr, 1) -> expr
    | Add (expr1, expr2) -> Add (simplify1 expr1, simplify1 expr2)
    | Mul (expr1, expr2) -> Mul (simplify1 expr1, simplify1 expr2)
    | Pow (expr, i) -> Pow (simplify1 expr, i)
    | expr -> expr

let rec simplify expr =
    let e = simplify1 expr in
        if expr = e
        then expr
        else simplify e



(* C.2 *)
let rec deriv var expr =
    match expr with
    | Int a' -> Int 0
    | Var x -> if x = var then Int 1 else Int 0
    | Add(a, b) -> Add ((deriv var a), (deriv var b))
    | Mul(a, b) -> Add (Mul(a, (deriv var b)), Mul((deriv var a), b))
    | Pow(a, b) -> Mul(Mul((Int b), Pow(a, b-1)),(deriv var a))



let derivative var expr =
    let e = simplify expr in
    let d = deriv var e in
        simplify d