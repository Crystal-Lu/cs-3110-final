(** 
   Data representation of Connect 4 Game. 

   This module contains represents the data stored in Connect 4 Game, including
   dots, player, and board. It also includes functions that update the states
   when dots were placed by a player.
*)

(** Information about the location of a dot, in row and column. *)
type dot

(** NOT SURE WHAT THIS DOES oops.. *)
type move = int

(** [player_type] is a player type, and it is either A or B. *)
type player_type = A | B

(** [space] indicates which dot is currently occupying a space in a board. *)
type space = A | B | Empty

(** Information about the current state, including the current dot arrangements
    on a board, current turn, and who has won the game. *)
type state

(** Information about a player, including the list of dots that the player has 
    placed so far and whether a player has won the game. *)
type player

(** [result] shows if the move is legal or not. It is illegal if a player has 
    attempted to add a dot to an already full column. *)
type result = Legal of state | Illegal

(** Raised when a player has attempted to add a dot to an already full column *)
exception Full

(** Raised when a player wins. *)
exception Win

(** Raised when the gameboard is full. *)
exception FullBoard

(** [empty_board] is a board that has not been occupied yet. *)
val empty_board : space array array

val full_state : state

(** [initial_state] initializes the state of the game. *)
val initial_state : unit -> state

(** [current_player] is the player who can currently place the dot. *)
val current_player : state -> player_type

(** [player_to_int] converts the player_type to string. *)
val player_to_int : player_type -> string

(** [check_win] checks if the player has won the game. *)
(* val check_win : *)

(** [full_column] checks if the column of the board is full, returning
    true if it's full and false if it's not full.
    Raises [Full] if the column is all occupied.
*)
val full_column : state -> int -> bool 

(** [full_row] checks if the row of the board is full, returning true
    if it's full and false if it's not full. *)
val full_row : state -> int -> bool

(** [empty_spot] takes in a column number and returns the row number of the  
    next empty spot in that column. *)
val empty_spot : state -> int -> int -> int

(** [check_legal] checks if the player's next move is a legal move. *)
val check_legal : state -> int -> bool

(** [put_dot] places a dot at a certain position. Updates the state
    of the user as well.  *)
val put_dot : state -> int -> state

(** [current_board st] is the current board layout.  *)
val current_board : state -> space array array

(** [full_board st] checks if the current board is filled with dots.
    It returns true if the board is full and false if not. 
    Raises [Full] if the board is all occupied.
*)
(* val full_board : state -> bool *)


(** [update_board st n player] updates the board. *)
val update_board : state -> int -> space -> space array array

(** [check_four st n] looks for four consecutive dots. *)
val check_four : state -> int -> bool

(** TODO *)
val change_player : state -> state