#use "common.ml"

module NFA = struct
  let make_trans l =
    let trans s c =
      let rec loop s c ~l =
        match l with
          [] -> IntSet.empty
          | ((s', c'), l) :: t ->
            if (s', c') = (s, c) then l
            else loop s c t
      in
      (loop s c l)
    in
    trans

  (*
    computes the epsilon closure for a set of states ss.
    The function discovers the states reachable from a given
    state s through epsilon transitions and transitively  computes all reachable
    states through subsequent epsilon transitions.
  *)

  let epsilon_closure ss trans =
    (*
      Loop to pop an element from stack and add new set of states.
    *)
    let rec loop ep stack =
      if MyStack.is_empty stack then ep
      else
        (* s: popped state; stack': modified stack *)
        let s, stack' = MyStack.pop stack in
        (* ns: next states reachable through a single e-transition from s *)
        let ns = trans s "" in 
        if ns = IntSet.empty then loop ep stack'
        else
          begin 
            let new_ss = IntSet.diff ns ep in
            let stack'' = MyStack.push_all stack' new_ss
            and ep' = MySet.add_set ep ns in
            loop ep' stack''
          end
    in
    let stack0 = MyStack.empty () in
    let stack = MyStack.push_all stack0 ss  (* initialise stack *)
    and ep0 = IntSet.empty in
    let ep = MySet.add_set ep0 ss in        (* initialise ep    *)
    loop ep stack

  type nfa = {
    states : IntSet.t;
    trans  : int -> bytes -> IntSet.t;
    s0     : int;
    final  : IntSet.t;
  }

  let dtrans n c state =
    let s = " " in
    Bytes.set s 0 c;
    let ns = n.trans state s in
    epsilon_closure ns n.trans

  let simNFA n s =
    let len = Bytes.length s in
    let rec loop ss p =
      (* Input string consumed completely. *)
      if p = len then
        not (IntSet.is_empty (IntSet.inter ss n.final))
      (* No new states resulted. *)
      else if ss = IntSet.empty then false
      (* Some states to process and input string not consumed completely yet. *)
      else
        let sslist = IntSet.elements ss in
        let nslist = List.map (dtrans n s.[p]) sslist in
        let ns = List.fold_left (fun s ss -> IntSet.union s ss) IntSet.empty nslist in
        loop ns (p + 1)
    in
    let ss0 = IntSet.of_list [n.s0] in
    let ss0' = epsilon_closure ss0 n.trans in
    loop ss0' 0
end

let t1 () =
  print_endline "running t1 ...";
  let trans = NFA.make_trans
    [
      ((1, ""), IntSet.of_list [4]);
      ((2, ""), IntSet.of_list [3]);
      ((3, ""), IntSet.of_list [4; 5]);
    ]
  and init = IntSet.of_list [1; 2] in
  let ep = NFA.epsilon_closure init trans in
  print_endline (MySet.string_of_set ep)

let t2 () =
  print_endline "running t2 ...";
  let trans =  NFA.make_trans
    [
      ((0, "a"), IntSet.of_list [0; 1]);
      ((0, "b"), IntSet.of_list [0]);
      ((1, "b"), IntSet.of_list [2]);
      ((2, "b"), IntSet.of_list [3]);
    ]
  and states = IntSet.of_list [0; 1; 2; 3]
  and s0 = 0
  and final = IntSet.of_list [3] in
  let n = { NFA.states = states; NFA.s0 = s0; NFA.trans = trans; NFA.final = final }
  and inputs = [ "aabbabb"; "abab" ] in
  let print i = print_endline (string_of_bool (NFA.simNFA n i)) in
  List.iter print inputs

let t3 () =
  print_endline "running t3 ...";
  let trans =  NFA.make_trans
    [
      ((0,  ""), IntSet.of_list [1; 7]);
      ((1,  ""), IntSet.of_list [2; 4]);
      ((2, "a"), IntSet.of_list [3]);
      ((3,  ""), IntSet.of_list [6]);
      ((4, "b"), IntSet.of_list [5]);
      ((5,  ""), IntSet.of_list [6]);
      ((6,  ""), IntSet.of_list [1; 7]);
      ((7, "a"), IntSet.of_list [8]);
      ((8, "b"), IntSet.of_list [9]);
      ((9, "b"), IntSet.of_list [10]);
    ]
  and states = IntSet.of_list [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
  and s0     = 0
  and final  = IntSet.of_list [10] in
  let n      = {
    NFA.states = states;
    NFA.s0     = s0;
    NFA.trans  = trans;
    NFA.final  = final
  }
  and inputs = [ "aabbabb"; "abab" ] in
  let print i = print_endline (string_of_bool (NFA.simNFA n i)) in
  List.iter print inputs

let main () =
  t1 ();
  t2 ();
  t3 ()

let _ = main ()
