open Data
open Command
open ANSITerminal
open Graphics

(** [board_space player] determines how each space is filled.
    "A" represents Player A, "B" represents Player B, and
    ASCII code 178 represents a blank space. *)
let board_space space row column =
  match space.(row).(column) with
  | A -> ANSITerminal.(print_string [red] " A ")
  | B -> ANSITerminal.(print_string [blue] " B ")
  | Empty -> ANSITerminal.(print_string [white]" O ")

(* [print_helper space row] is a helper function that prints A if player A's dot
   is located, and prints B if player B's dot is located. If a space is 
   unoccupied, then O is filled. *)
let print_helper space row =
  match space.(row) with
  | A -> ANSITerminal.(print_string [red] " A ")
  | B -> ANSITerminal.(print_string [blue] " B ")
  | Empty -> ANSITerminal.(print_string [white] " O ")

(** [print_board matrix] does pretty-printing of the game board into utop with
    ASCII characters. *)
let print_board matrix =

  (* Create a loop that runs through the array, and calls
      [board_space] to determine how the space is filled. *)

  (* Top-down implementation, starting with top row and left-most
      column. *)
  ANSITerminal.(print_string [white] ("\n       Column       \n"));
  ANSITerminal.(print_string [white] (" 1  2  3  4  5  6  7 \n\n"));
  for row = 5 downto 0 do
    for column = 0 to 6 do
      board_space matrix row column
    done;
    print_newline ();
  done;
  print_newline ()