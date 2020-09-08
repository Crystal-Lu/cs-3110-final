(** type [column] indicates the location of the column. *)
type column = int

(** type [command] are kinds of commands that can be interpreted as. *)
type command = 
  |Start 
  |Quit
  |Place of column (* column ranges from 1 to 7 *)

(** Raised when an entered command is not well-formatted. *)
exception Malformed

(* [remove l v] removes all elements with a value of v. *)
let rec remove l v = 
  match l with
  |[] -> []
  |h :: t -> if (h = v) then remove t v 
    else h :: remove t v

(* [parse_match l] matches the first element of the filtered list. *)
let parse_match l = 
  let filtered = remove l "" in 
  match List.nth filtered 0 with 
  |"start" -> if (List.length filtered != 1) then raise Malformed 
    else Start
  |"quit" -> if (List.length filtered != 1) then raise Malformed 
    else Quit
  |"1" -> Place (int_of_string (List.nth filtered 0))
  |"2" -> Place (int_of_string (List.nth filtered 0))
  |"3" -> Place (int_of_string (List.nth filtered 0))
  |"4" -> Place (int_of_string (List.nth filtered 0))
  |"5" -> Place (int_of_string (List.nth filtered 0))
  |"6" -> Place (int_of_string (List.nth filtered 0))
  |"7" -> Place (int_of_string (List.nth filtered 0))
  |_ -> raise Malformed

(** [parse str] parses [str] into Start, Quit, or Place. If the command is not
    well-formatted, then it raises [Malformed]. *)
let parse str = 
  try 
    let trimmed = String.trim str in 
    if (String.length trimmed) = 0 then raise Malformed
    else 
      let lowercase = String.lowercase_ascii trimmed in 
      let splitted_list = String.split_on_char ' ' lowercase in 
      parse_match splitted_list
  with  
  |_ -> raise Malformed

