#use "buffer.ml"

type state =
    Terminate of bool
  | State of (char -> state)

let one_zero () =
  let rec init c =
    if c = '1' then State(accept) else Terminate(false)

  and accept c =
    if c = '0' then State(init)
    else if c = '1'  then State(accept)
    else Terminate(false)
  in
  (State(init), [accept])

let run_one_zero dfa buffer =
  let (init, accept_states) = one_zero () in
  let rec loop sS = 
    print_endline "Looping ...";
    match sS with
      Terminate(tf) -> tf
    | State(s) ->
        try
          let c = buffer () in
          loop (s c)
        with
          End_of_buffer -> List.mem s accept_states
  in
  loop init
