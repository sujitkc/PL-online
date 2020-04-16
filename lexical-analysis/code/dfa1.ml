#use "common.ml"

module DFA = struct
  type dfa = {
    states : IntSet.t;
    trans  : int -> bytes -> int option;
    s0     : int;
    final  : IntSet.t;
  }

  let make_trans l =
    let trans s c =
      let rec loop s c ~l =
        match l with
          [] -> None
          | ((s', c'), l) :: t ->
            if (s', c') = (s, c) then Some(l)
            else loop s c t
      in
      (loop s c l)
    in
    trans

  let simDFA d s =
    let rec loop curr_state pos =
      if pos = Bytes.length s then
        (IntSet.mem curr_state d.final)
      else
        match (d.trans curr_state (string_of_char s.[pos])) with
          None -> false
        | Some(s') -> loop s' (pos + 1)
    in
    loop d.s0 0
end

let t2 () =
  print_endline "running t2 ...";
  let trans =  DFA.make_trans
    [
      ((0, "a"), 1);
      ((0, "b"), 0);
      ((1, "a"), 1);
      ((1, "b"), 2);
      ((2, "a"), 1);
      ((2, "b"), 3);
      ((3, "b"), 0);
      ((3, "a"), 1);
    ]
  and states = IntSet.of_list [0; 1; 2; 3]
  and s0 = 0
  and final = IntSet.of_list [3] in
  let n = { DFA.states = states; DFA.s0 = s0; DFA.trans = trans; DFA.final = final }
  and inputs = [ "aabbabb"; "abab" ] in
  let print i = print_endline (string_of_bool (DFA.simDFA n i)) in
  List.iter print inputs

let main () =
  t2 ()

let _ = main ()
