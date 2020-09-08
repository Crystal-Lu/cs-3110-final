open Data
open Board

type 'a tree =
  | Leaf of 'a
  | Node of 'a * 'a tree list

let node = function
  | Leaf (a) -> a
  | Node (a,_) -> a

let rec available_moves state num list =
  if (num = 0) then list else
    match (Data.check_legal state num) with
    | true -> available_moves state (num-1) (num::list)
    | false -> available_moves state (num-1) list

let check_four_ai state n =
  (n,(check_four state n))

let rec match_moves state moves rev =
  match moves with
  | (x,y)::t -> if y then
      (if rev then (put_dot (Data.change_player state) x) else (put_dot state x)) else (match_moves state t rev)
  | [] -> raise Not_found

(* Rules for AI:
    1. If there's a winning move, take it.
    2. If your opponent has a winning move, take it.
    3. Take the center square over edges and corners.
    4. Take the corner square over edges.
    5. Take the edges if they're the only thing available. *)

let simple_ai state =
  let moves = available_moves state 7 [] in
  if (moves <> []) then
    try
      match_moves state (List.map (check_four_ai state) moves) false
    with
      Not_found -> 
      try
        match_moves (Data.change_player state) (List.map (check_four_ai state) moves) true
      with
        Not_found ->
        if (check_legal state 3) then (put_dot state 3)
        else if (check_legal state 2) then (put_dot state 2)
        else if (check_legal state 4) then (put_dot state 4)
        else if (check_legal state 1) then (put_dot state 1)
        else if (check_legal state 5) then (put_dot state 5)
        else if (check_legal state 0) then (put_dot state 0)
        else put_dot state 6
  else raise FullBoard