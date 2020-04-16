
(* module type MyIntSig = Set.Comparable with type t = int *) 

module MyInt = struct
  type t = int
  let compare x y =
    if x < y then -1
    else if x = y then 0
    else 1
end

module IntSet = Set.Make(MyInt)

module type MyStackSig = sig
  type t
  type elt

  val empty : unit -> t
  val is_empty : t -> bool
  val push : elt -> t -> t
  val pop : t -> elt * t
  val push_all : t -> IntSet.t -> t
  val string_of_stack : t -> bytes
end

module type MyIntStackSig = MyStackSig with type elt = int

let string_of_list lst =
  let rec loop lst =
    match lst with
      [] -> ""
    | h :: t -> (string_of_int h) ^ "; " ^ (loop t)
  in
  "[" ^ (loop lst) ^ "]"

module MyStack : MyIntStackSig = struct
  type elt = int
  type t = int list

  let empty () = []
  let is_empty stack = stack = []
  let push el stack = el :: stack
  let pop = function
      [] -> failwith "Empty stack!"
    | h :: t -> (h, t)

  let push_all stack ss =
    let lst = IntSet.elements ss in
    let rec loop stack = function
        [] -> stack
      | h :: t -> loop (push h stack) t
    in
    loop stack lst

  let rec string_of_stack = string_of_list
end

module MySet = struct
  let add_list set lst = 
    let rec loop set' = function
        [] -> set'
      | h :: t -> loop (IntSet.add h set') t
    in
    loop set lst

  let add_set set1 set2 =
    let l2 = IntSet.elements set2 in
    add_list set1 l2

  let string_of_set s =
    string_of_list (IntSet.elements s)
end

let string_of_char c =
  let s = " " in Bytes.set s 0 c; s
