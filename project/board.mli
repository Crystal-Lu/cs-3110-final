(** [board_space player] determines how each space is filled.
    "A" represents Player A, "B" represents Player B, and
    ASCII code 178 represents a blank space. *)
val board_space : Data.space array array -> int -> int -> unit

(** [print_board] does pretty-printing of the game board into utop with
    ASCII characters. *)
val print_board : Data.space array array -> unit