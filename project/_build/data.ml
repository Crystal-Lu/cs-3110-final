(* File includes each player's state and functions that change or indicate the 
   state *)

let width = 7
let size = 6

type dot = 
  {
    row: int;
    column: int;
  }

type player_type = A | B

type move = int
type space = A | B | Empty
type board = space array array


type state = {
  board: board;
  turn: player_type;
  win: space;
}
type player = {
  dots: dot list;
  win: int; (* 1 would indicate that the person has won the game *)
}
type result = Legal of state | Illegal
exception Full
exception Win
exception FullBoard

let empty_board = Array.make_matrix 6 7 Empty (* maybe delete *)
let full_board = Array.make_matrix 6 7 (A:space)

(* PLAYER FUNCTIONS *)

(** [initial_state] initializes the state of the game. *)
let initial_state () = 
  {board = Array.make_matrix 6 7 Empty;
   turn = A;
   win = Empty;}

let full_state =
  {board = full_board;
   turn = A;
   win = Empty;}

(** [current_player] is the player who can currently place the dot. *)
let current_player st =
  st.turn

(** [current_board st] shows the current board layout.  *)
let current_board st =
  st.board

let player_to_space (p_type:player_type) =
  match p_type with
  |(A:player_type) -> A
  |(B:player_type) -> B

let player_to_int (p_type:player_type) =
  match p_type with
  |A -> "A"
  |B -> "B"

(** [full_column] checks if the column of the board is full raising Full and false if it's not full. 
    Raises [Full] if the column is all occupied.
*)
exception Full_Column
let full_column st column =
  try
    (if (st.board.(5).(column) = Empty) then false else true)
  with Invalid_argument(_) -> raise Full_Column

(** [full_row] checks if the row of the board is full, raising Full
    if it's full and false if it's not full. *)
exception Full_Row
let full_row st row=
  try
    if (Array.for_all ((<>) Empty) st.board.(row)) then true else false
  with Invalid_argument(_) -> raise Full_Row
(** Unsure if this function or the next are necessary.
    It looks like you already wrote them below, and I
    wrote these before I noticed. *)


(* =======  *)

(* BOARD FUNCTIONS *)
(** [current_board st] shows the current board layout.  *)
let current_board st =
  st.board

(** [full_board st] checks if the current board is filled with dots.
    It returns true if the board is full and false if not.
    Raises [Full] if the board is all occupied.
*)
let full_board mat =
  for row = 0 to 5 do
    if (full_row mat row) then raise Full
  done;
  false

let rec find_empty_spot_helper l =
  match l with 
  |[] -> failwith "Empty column"
  |h :: t -> if h = Empty then h else find_empty_spot_helper t

(* [find_empty_spot st c] finds the lowest empty spot (in row number) of [c]th column of the board.
   Basically, Empty that is closest to the end of the array is the "lowest" empty 
   spot. *)
exception Find_Empty_Spot
let rec find_empty_spot st c r =
  try
    (let board = current_board st in 
     if (r>5) then raise Full
     else if (r < 0) then raise Full
     else if ((board.(r).(c) = Empty) && r <= 5) then r 
     else find_empty_spot st c (r+1))
  with Invalid_argument (_) -> raise Find_Empty_Spot

let empty_spot st c r =
  find_empty_spot st c r

exception Last_Helper
let rec last_helper state column row =
  try
    (if (row>5) then 5 else
     if (row<0) then 0 else
     if (state.board.(row).(column) <> Empty) then row else last_helper state column (row-1))
  with Invalid_argument (_) -> raise Last_Helper

let last_dot_placed state column =
  last_helper state column 5

(** [update_board st c player] updates the board. Remakes the board with new dot *)
(* TODO: make this return a board. *)
exception Update_Board_Helper
let update_board_helper st c player =
  try
    (let original_board = current_board st in 
     let row = find_empty_spot st c 0 in 
     original_board.(row).(c) <- player)
  with Invalid_argument (_) -> raise Update_Board_Helper

let update_board st c player = 
  update_board_helper st c player;
  current_board st
(* not sure if this function actually works *)

(** [check_legal] checks if the player's next move is a legal move. *)
exception Check_Legal
let check_legal st column = 
  try
    (if (column < 0 || ((Array.length (current_board st)) < column)) then false else
       try
         if (full_column st column) then false else true
       with
         Full -> false)
  with Invalid_argument (_) -> raise Check_Legal

(* let win = failwith "Unimplemented" *)

(* whoa this is really confusing sorry!
     I'll try explain how this works:
     The count represents how many connected spaces of the player type have
     been found already. When it reaches 4, that means there's a four-of-a-kind.
     Then I check if the column is within bounds (1 through 7). If so, I then
     check if the current space is the player type. If so, I check the direction
     I'm travelling. If it's positive, I increase the count and column by one and run it again. If it's negative, I add 1 to count and subtract 1 from column. If the current space isn't the player type, I check direction again. If it's positive, I turn it negative and we go the other way now. If it's negative, the loop ends here.
*)

(** [check_row st n player count col dir org] returns true if
    there is a four-of-a-kind and false if there is not. *)
exception Check_Row
exception One
let rec check_row st row (player:space) count col dir org = 
  try
    (if (count=4) then true else
     if (col < 0) then false else
     if (col > 6) then (check_row st row player count (org-1) (-1) org) else
     if (st.board.(row).(col) = player) then
       try
         (if (dir > 0) then (check_row st row player (count+1) (col+1) 1 org)
          else (check_row st row player (count+1) (col-1) (-1) org))
       with Invalid_argument (_) -> raise One
     else if (dir > 0) then (check_row st row player count (org-1) (-1) org) else if (dir < 0) then false else false)
  with Invalid_argument (_) -> raise Check_Row

exception Check_Column
let rec check_column st row player count col dir org =
  try
    (if (count = 4) then true else
     if (row < 0) then false else
     if (row > 5) then (check_column st (org-1) player count col (-1) org) else
     if (st.board.(row).(col) = player) then
       if (dir > 0) then (check_column st (row+1) player (count+1) col 1 org)
       else (check_column st (row-1) player (count+1) col (-1) org)
     else if (dir > 0) then (check_column st (org-1) player count col (-1) org) else false)
  with Invalid_argument (_) -> raise Check_Column

exception Left_Diagonal
let rec check_left_diagonal st row player count col dir org_row org_col =
  try
    (if (count = 4) then true else
     if (row > 5 || col < 0) then (check_left_diagonal st (org_row-1) player count (org_col +1) (-1) org_row org_col) else
     if (row < 0 || col > 6) then false else
     if (st.board.(row).(col) = player) then
       if (dir > 0) then (check_left_diagonal st (row+1) player (count+1) (col-1) 1 org_row org_col) else (check_left_diagonal st (row-1) player (count+1) (col+1) (-1) org_row org_col)
     else if (dir > 0) then (check_left_diagonal st (org_row-1) player count (org_col+1) (-1) org_row org_col) else false)
  with Invalid_argument (_) -> raise Left_Diagonal

exception Right_Diagonal
let rec check_right_diagonal st row player count col dir org_row org_col =
  try
    (if (count = 4) then true else
     if (row > 5 || col > 6) then (check_right_diagonal st (org_row-1) player count (org_col-1) (-1) org_row org_col) else
     if (row < 0 || col < 0) then false else
     if (st.board.(row).(col) = player) then
       if (dir > 0) then (check_right_diagonal st (row+1) player (count+1) (col+1) 1 org_row org_col) else (check_right_diagonal st (row-1) player (count+1) (col-1) (-1) org_row org_col)
     else if (dir > 0) then (check_right_diagonal st (org_row-1) player count (org_col-1) (-1) org_row org_col) else false)
  with Invalid_argument(_) -> raise Right_Diagonal

(** [check_four] looks for four consecutive dots. *)
let check_four st n =
  try
    (if (check_row st (last_dot_placed st n) (st |> current_player |> player_to_space) 0 n 1 n) then true else
     if (check_column st (last_dot_placed st n) (st |> current_player |> player_to_space) 0 n 1 (last_dot_placed st n)) then true else
     if (check_left_diagonal st (last_dot_placed st n) (st |> current_player |> player_to_space) 0 n 1 (last_dot_placed st n) n) then true else
     if (check_right_diagonal st (last_dot_placed st n) (st |> current_player |> player_to_space) 0 n 1 (last_dot_placed st n) n) then true else false)
  with Full ->
    (  if (check_row st 5 (st |> current_player |> player_to_space) 0 n 1 n) then true else
       if (check_column st 5 (st |> current_player |> player_to_space) 0 n 1 5) then true else
       if (check_left_diagonal st 5 (st |> current_player |> player_to_space) 0 n 1 5 n) then true else
       if (check_right_diagonal st 5 (st |> current_player |> player_to_space) 0 n 1 5 n) then true else false)


let put_dot_helper st n = 
  (check_four st n = false) && (check_legal st n)

(** [put_dot st n] places a dot on nth column. Updates the state
    of the game as well. ROUGH DRAFT can reduce this if we use check four on
    main*)
let put_dot (st:state) n = 
  if (check_legal st n) && (current_player st = A) then
    (let b = (update_board st n A) in
     let s = {board = b; 
              turn = A;
              win = Empty;
             }
     in if (check_four s n) then raise Win else
       {board = b;
        turn = B;
        win = Empty}
    )
  else if (put_dot_helper st n) && (current_player st = B) then
    (let b = update_board st n B in
     let s = {board = b; 
              turn = B;
              win = Empty;
             }
     in if (check_four s n) then raise Win else 
       {board = b;
        turn = A;
        win = Empty})
  else if (check_legal st n) = false then 
    st
  else if full_board st then raise FullBoard
  else raise Win


let change_player state =
  match (state.turn) with
  | A -> {board = state.board;
          turn = B;
          win = state.win}
  | B -> {board = state.board;
          turn = A;
          win = state.win}