(**
   A module that parses user commands entered in terminal.
*)

type column = int

type command = 
  |Start 
  |Quit
  |Place of column

(** Raised when an entered command is not well-formatted. *)
exception Malformed


(** [parse str] parses [str] into Start, Quit, or Place. If the command is not
    well-formatted, then it raises [Malformed]. *)
val parse : string -> command


