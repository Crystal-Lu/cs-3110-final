open Data
open Board
open Command
open ANSITerminal
open Ai
open Graphics
open Gui


let rec play_game () : unit =
  ANSITerminal.(print_string [white] "\nWelcome to the Connect Four Game!\n");
  ANSITerminal.(print_string [white] "\nPlease choose the number of players: 1 
  or 2. If you'd like to use the graphics module, please type 'GUI'. \n> ");
  let input = read_line() in 
  if input = "1" || input = "one" then 
    game_one (Data.initial_state ()) false
  else if input = "2" || input = "two" then 
    game_two (Data.initial_state ())
  else if input = "GUI" || input = "gui" then
    (gui_one (Data.initial_state ()) (0) true)
  else (
    ANSITerminal.(print_string [white] "\nTry again!\n");
    play_game ())

(* one-player game *)
and game_one state ai =
  (Board.print_board (Data.current_board state));
  ANSITerminal.(print_string [white] ("\nPlease write the column of the move you
   wish to play.\n" ^ "Or, type quit if you'd like to stop the game.\n\n> "));
  if (ai) then 
    let ai_state = Ai.simple_ai state in
    try 
      game_one (ai_state) false
    with
      Win -> 
      (Board.print_board (Data.current_board ai_state);
       ANSITerminal.(print_string [white] ("Sorry, you lost!")))
  else try
      match parse (read_line ()) with
      |Quit ->
        ANSITerminal.(print_string [white] ("\nIt was nice having you! We hope 
        you enjoyed our game.\n"));
        exit 0;
      |Place (n) -> 
        (ANSITerminal.(print_string [white] ("\nDot placed in column " ^ 
                                             string_of_int(n) ^ ".\n\n"));
         let win_state = Data.put_dot state (n-1) in
         try
           game_one (win_state) true
         with
           Win ->
           Board.print_board (Data.current_board win_state);
           ANSITerminal.(print_string [white] ("Congrats, 
           Player " ^ (Data.player_to_int (current_player state)) ^ " won!\n")); 
           ANSITerminal.(print_string [white] ("\nDo you want to play it again? 
           Type 'y' to play again.: ")); 
           let input = read_line () in
           if (input) = "y" || (input) = "Y" then play_game ()
           else ANSITerminal.(print_string [white] ("\nIt was nice having you! 
           We hope you enjoyed our game.\n"));
           exit 0;)
      (* TODO: add call to func that changes state and board*)
      (* TODO: NEED TO CHECK FOR FULL BOARD + TIES *)
      |Start ->
        ANSITerminal.(print_string [white] ("\nThe game's already started!\n
        Please type in the column where you want to place the dot.\n\n"));
        game_two state
    with
    |Malformed ->
      ANSITerminal.(print_string [white] ("\nSorry, your command was 
      undecipherable!\n Please write the column in which you'd like to place to
       dot.\n\n"));
      game_two state
    |Full ->
      ANSITerminal.(print_string [white] ("\nSorry, the column you chose is 
      full. Please choose another column.\n"));
      game_two state
    |Win ->
      ANSITerminal.(print_string [white] ("Congrats, 
      Player " ^ (Data.player_to_int (current_player state)) ^ " won!"))

(* two-player game *)
and game_two state = 
  (Board.print_board (Data.current_board state));
  ANSITerminal.(print_string [white] ("\nPlease write the column of the move you
   wish to play.\n" ^ "Or, type quit if you'd like to stop the game.\n\n> "));
  try
    match parse (read_line ()) with
    |Quit ->
      ANSITerminal.(print_string [white] ("\nIt was nice having you! We hope 
      you enjoyed our game.\n"));
      exit 0;
    |Place (n) -> 
      (ANSITerminal.(print_string [white] ("\nDot placed in column " ^ 
                                           string_of_int(n) ^ ".\n\n"));
       try
         game_two (Data.put_dot state (n-1))
       with 
       |FullBoard-> ANSITerminal.(print_string [white] ("It's a tie!"));
       | Win -> ANSITerminal.(print_string [white] ("Congrats, 
       Player " ^ (Data.player_to_int (current_player state)) ^ " won!\n"))); 
      ANSITerminal.(print_string [white] ("\nDo you want to play it again? 
      Type 'y' to play again.: ")); 
      let input = read_line () in
      if (input) = "y" || (input) = "Y" then play_game ()
      else ANSITerminal.(print_string [white] ("\nIt was nice having you! 
      We hope you enjoyed our game.\n"));
      exit 0;
      (* TODO: add call to func that changes state and board*)
      (* TODO: NEED TO CHECK FOR FULL BOARD + TIES *)
    |Start ->
      ANSITerminal.(print_string [white] ("\nThe game's already started!\n
      Please type in the column where you want to place the dot.\n\n"));
      game_two state
  with
  |FullBoard-> ANSITerminal.(print_string [white] ("It's a tie!"));
  |Malformed -> ANSITerminal.(print_string [white] ("\nSorry, your command was 
  undecipherable!\n Please write the column in which you'd like to place to dot.
  \n\n"));
    game_two state
  |Full ->
    ANSITerminal.(print_string [white] ("\nSorry, the column you chose is full.
     Please choose another column.\n"));
    game_two state
  |Win ->
    ANSITerminal.(print_string [white] ("Congrats, 
    Player " ^ (Data.player_to_int (current_player state)) ^ " won!"))


and gui_one state move repeat = 
  if (repeat=false) then Gui.draw_board state move else ();
  ANSITerminal.(print_string [white] ("\nPlease write the column of the 
  move you wish to play.\n" ^ "Or, type quit if you'd like to stop the game.
  \n\n> "));
  try
    match parse (read_line ()) with
    |Quit ->
      ANSITerminal.(print_string [white] ("\nIt was nice having you! We hope
       you enjoyed our game.\n"));
      exit 0;
    |Place (n) -> 
      (ANSITerminal.(print_string [white] ("\nDot placed in column " ^
                                           string_of_int(n) ^ ".\n\n"));
       try
         gui_one (Data.put_dot state (n-1)) (n-1) false
       with
         Win -> ANSITerminal.(print_string [white] ("Congrats, 
         Player " ^  (Data.player_to_int (current_player state)) ^ " won!\n")));
      Graphics.close_graph ();
      Graphics.open_graph " 364x312";
      ANSITerminal.(print_string [white] ("\nDo you want to play it again?
       Type 'y' to play again.: ")); 
      let input = read_line () in
      if (input) = "y" || (input) = "Y" then play_game ()
      else ANSITerminal.(print_string [white] ("\nIt was nice having you! We
       hope you enjoyed our game.\n"));
      exit 0;
      (* TODO: add call to func that changes state and board*)
      (* TODO: NEED TO CHECK FOR FULL BOARD + TIES *)
    |Start ->
      ANSITerminal.(print_string [white] ("\nThe game's already started!\nPlease
       type in the column where you want to place the dot.\n\n"));
      gui_one state (0) true
  with
  |FullBoard-> ANSITerminal.(print_string [white] ("It's a tie!"));
  |Malformed ->
    ANSITerminal.(print_string [white] ("\nSorry, your command was
     undecipherable!\n Please write the column in which you'd like to place 
     to dot.\n\n"));
    gui_one state (0) true
  |Full ->
    ANSITerminal.(print_string [white] ("\nSorry, the column you chose is full.
     Please choose another column.\n"));
    gui_one state (0) true
  |Win ->
    ANSITerminal.(print_string [white] ("Congrats, Player
     " ^ (Data.player_to_int (current_player state)) ^ " won!"))


let () = play_game ()