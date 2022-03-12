(* A.1 *)
let rec last_sublist = function
    | [] -> invalid_arg "last_sublist: empty list"
    | [h] -> [h]
    | h :: t -> last_sublist t

(* A.2 *)
let reverse lst =
    let rec iter lst tail =
        match tail with
        | [] -> lst
        | h :: t -> iter (h::lst) t
    in iter [] lst

(* A.3 *)
let rec square_list = function
  | [] -> []
  | h :: t -> (h * h) :: square_list t

let square_list2 items = List.map (fun x -> x * x) items

(* A.4 *)
(*  This doesn't work because Louis squares every element and 
    puts it at the beginning of the list, so his list is in the
    reverse order.
    
    This doesn't work because to the left of :: we need to have an 
    element of the list and to the right, we know that we need a list
    and (h * h) is not a list.

    Yes, we can modify it to (answer @ [h* h]), but it would not be
    efficient because the @ operand is not tail recursive. The @
    opperand would require a copy to mde of the left list which would
    result in a space and time complexity of O(n) instead of constant O(1) so it
    requires more space and more time and therefore is less efficient.
    
     *)
let square_list items =
    let rec iter things answer =
        match things with
        | [] -> answer
        | h :: t -> iter t (answer @ [h * h])
    in iter items []

(* A.5 *)
let count_negative_numbers lst =
    let rec iter lst n =
        match lst with
        | [] -> n
        | h :: t -> if h < 0 then iter t (n + 1) else iter t n in
    iter lst 0

(* A.6 *)
let pow x y =
    if y = 0 then 1 else
    let rec iter ans x y =
        if y = 1 then ans else iter (ans * x) x (y-1) 
    in iter x x y

let power_of_two_list n = 
    if n = 0 then [] else
    let rec iter lst n =
        if n-1 = 0 then 1 :: lst
        else iter (pow 2 (n-1) :: lst) (n-1)
    in iter [] (n)

(* A.7 *)
let prefix_sum lst = 
    let rec iter lst temp sum =
        match lst with
        | [] -> temp
        | [i] -> (sum + i) :: temp
        | h :: t -> iter t ((sum + h) :: temp) (sum + h)
    in reverse(iter lst [] 0)

(* A.8 *)
let deep_reverse lst =
    let rec iter lst temp =
        match lst with
        | [] -> temp
        | h :: t -> iter t (reverse h :: temp) in
    iter lst []
(* A.9 *)
 type 'a nested_list =
    | Value of 'a
    | List of 'a nested_list list

let rec deep_reverse_nested lst =
    let reverses l =
        let rec iter lst temp = match lst with
            | [] -> temp
            | h :: t -> iter t ((deep_reverse_nested h) :: temp)
        in iter l []
    in
    match lst with
    | Value x -> Value x
    | List l -> List (reverses l)

(* B.1 *)
let rec quicksort lst cmp =
	match lst with
		| [] -> []
		| h :: t -> 
			let sm = List.filter (fun n -> cmp n h) t in
			let lg = List.filter (fun n -> not (cmp n h)) t in
				(quicksort sm cmp) @ (h :: (quicksort lg cmp))

(* B.2 *)
(*
    The quicksort function is an instance of generative recursion 
    and not structural recursion because it generates sections from 
    the list of elements that greater than or equal to the pivot, 
    and the recurses on every section and then combines the sections
    for a final result. This is not structural recursion because the
    original list is not modified.
 *)

 (* B.3 *)
 (*
    This will not work because the function will enter an infinite 
    recursion loop because in the case of a list having one element,
    we will have to split our element into two halves, one with [x] and 
    another with [], thus we wll continue splitting like this infintely.
  *)

 (* B.4 *)
 let rec insert_in_order new_result a_list cmp =
    match a_list with
        | [] -> [new_result]
        | h :: t when cmp new_result h -> new_result :: a_list
        | h :: t ->  h :: insert_in_order new_result t cmp

let rec insertion_sort a_list cmp =
    match a_list with
        | [] -> []
        | h :: t -> insert_in_order h (insertion_sort t cmp) cmp
(* 
    This is structural recursion because no new list is being created,
    instead we can see that we are separating the head and tail, sorting
    the tail and then inserting the head into the sorted tail. *)


(* C.1 *)
let rec subsets = function
    | [] -> [[]]
    | h :: t -> let rest = subsets t in
        rest @ (List.map (fun x -> h :: x) rest )
      
(* This works because to get our subsets, we want to check to see
    if we should add our element or not for every element of the set. 
    This way we can generate all of the subsets. *)

(* C.2 *)
let rec accumulate op initial sequence =
    match sequence with
        | [] -> initial
        | h :: t -> op h (accumulate op initial t)

let map p sequence =
    accumulate (fun x r -> (p x) :: r) [] sequence

let append seq1 seq2 =
    accumulate (fun x r -> x :: r) seq2 seq1

let length sequence =
    accumulate (fun x r -> r + 1) 0 sequence

(* C.3 *)
let rec accumulate_n op init seqs =
    match seqs with
        | [] -> failwith "empty list"
        | [] :: _ -> []   (* assume all sequences are empty *)
        | h :: t -> accumulate op init (List.map List.hd seqs) :: accumulate_n op init (List.map List.tl seqs)

(* C.4 *)
let rec map2 f x y =
    match (x, y) with
        | ([], []) -> []
        | ([], _) -> failwith "unequal lists"
        | (_, []) -> failwith "unequal lists"
        | (lh::lt, rh:: rt) -> (f lh rh) :: map2 f lt rt

let dot_product v w = accumulate (+) 0 (map2 ( * ) v w)

let matrix_times_vector m v = map(dot_product v) m

let transpose mat = accumulate_n (fun x y -> x::y) [] mat

let matrix_times_matrix m n =
    let cols = transpose n in
        map (fun r -> matrix_times_vector cols r) m