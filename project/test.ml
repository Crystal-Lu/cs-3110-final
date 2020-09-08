(**  
   Test Plan:
   We used both automated and manual testing. For automated testing, we used
   OUnit to compare expected vs computed values. command.ml and data.ml were 
   tested in this file (automated); modules not mentioned (ai.ml, board.ml, 
   main.ml) were tested on the terminal (manual). The tests in this file were 
   black-box testing, as all the test cases below were just comparing the 
   outputs, which was enough to test the functionality of the game. The testing 
   demonstrates the correctness of the system because it tests parse commands
   and game data, which is very hard to test it on terminal. All other modules,
   which can be visualized on the terminal, was able to be tested manually.
*)
open OUnit2
open Command
open Data
open Gui

let empty_board = Array.make_matrix 6 7 Empty
let full_board = Array.make_matrix 6 7 (A:space)


let parse_tests = [ (* 17 *)
  "parse_start_malformed" >:: (fun _ -> assert_raises (Malformed)
                                  (fun () -> parse ("   start    hello ")));
  "parse_malformed" >:: (fun _ -> assert_raises (Malformed)
                            (fun () -> parse ("   mademade ")));                                 
  "parse_start_ok1" >:: (fun _ -> assert_equal (Start)
                            (parse ("   start ")));
  "parse_start_ok2" >:: (fun _ -> assert_equal (Start)
                            (parse ("   Start ")));
  "parse_go1" >:: (fun _ -> assert_equal (parse (" 3 ")) (Place 3)); 
  "parse_go2" >:: (fun _ -> assert_equal (parse (" 5 ")) (Place 5)); 
  "parse_go3" >:: (fun _ -> assert_equal (parse ("4")) (Place 4)); 
  "parse_go_malformed0" >:: (fun _ -> assert_raises (Malformed)
                                (fun () -> parse ("0")));
  "parse_go_malformed1" >:: (fun _ -> assert_raises (Malformed)
                                (fun () -> parse ("Place 0"))); 
  "parse_go_malformed2" >:: (fun _ -> assert_raises (Malformed)
                                (fun () -> parse ("Place 8"))); 
  "parse_go_malformed3" >:: (fun _ -> assert_raises (Malformed)
                                (fun () -> parse ("8")));       
  "parse_go_malformed4" >:: (fun _ -> assert_raises (Malformed)
                                (fun () -> parse ("Place -1")));  
  "parse_go_malformed5" >:: (fun _ -> assert_raises (Malformed)
                                (fun () -> parse ("-1")));                    
  "parse_quit_malformed6" >:: (fun _ -> assert_raises (Malformed)
                                  (fun () -> parse ("   quit    quit ")));  
  "parse_quit_malformed7" >:: (fun _ -> assert_raises (Malformed)
                                  (fun () -> parse ("quitted")));                                
  "parse_quit_ok1" >:: (fun _ -> assert_equal (Quit)
                           (parse ("   quit ")));
  "parse_quit_ok2" >:: (fun _ -> assert_equal (Quit)
                           (parse ("   Quit ")));
] 

let update = (Data.put_dot (initial_state ()) 1) 
let update1 = (Data.put_dot (initial_state ()) 2)
let update2 = (Data.put_dot (initial_state ()) 2)

let data_tests = (* 34 *) [
  "full_column returns false with empty matrix" >:: 
  (fun _ -> assert_equal (full_column (initial_state( )) 1) false);
  "check four" >:: (fun _ -> assert_equal 
                       (check_four (initial_state ()) 0) false);
  "current player0" >:: (fun _ -> assert_equal 
                            (current_player (initial_state ())) A);
  "check four true" >:: (fun _ -> assert_equal 
                            (check_four Data.full_state 0) true);
  "current player1" >:: (fun _ -> assert_equal (current_player update) B);
  "current board1" >:: (fun _ -> assert_equal 
                           ((current_board update) = (empty_board)) false);
  "check row1" >:: (fun _ -> assert_equal (full_row update 0) false);
  "check col1" >:: (fun _ -> assert_equal (full_column update 0) false);
  "check row1.1" >:: (fun _ -> assert_equal (full_row update 1) false);
  "check col1.1" >:: (fun _ -> assert_equal (full_column update 1) false);
  "check four1" >:: (fun _ -> assert_equal (check_four update 0) false);
  "check four1.1" >:: (fun _ -> assert_equal (check_four update 1) false);
  "check legal1" >:: (fun _ -> assert_equal (check_legal update 0) true);
  "check legal1.1" >:: (fun _ -> assert_equal (check_legal update 1) true);

  "current player2" >:: (fun _ -> assert_equal (current_player update1) B);
  "current board2" >:: (fun _ -> assert_equal 
                           ((current_board update1) = (empty_board)) false);
  "check row2" >:: (fun _ -> assert_equal (full_row update1 0) false);
  "check col2" >:: (fun _ -> assert_equal (full_column update1 0) false);
  "check row2.1" >:: (fun _ -> assert_equal (full_row update1 1) false);
  "check col2.1" >:: (fun _ -> assert_equal (full_column update1 1) false);
  "check four2" >:: (fun _ -> assert_equal (check_four update1 0) false);
  "check four2.1" >:: (fun _ -> assert_equal (check_four update1 1) false);
  "check legal2" >:: (fun _ -> assert_equal (check_legal update1 0) true);
  "check legal2.1" >:: (fun _ -> assert_equal (check_legal update1 1) true);

  "current player3" >:: (fun _ -> assert_equal (current_player update2) B);
  "current board3" >:: (fun _ -> assert_equal 
                           ((current_board update2) = (empty_board)) false);
  "check row3" >:: (fun _ -> assert_equal (full_row update2 0) false);
  "check col3" >:: (fun _ -> assert_equal (full_column update2 0) false);
  "check row3.1" >:: (fun _ -> assert_equal (full_row update2 1) false);
  "check col3.1" >:: (fun _ -> assert_equal (full_column update2 1) false);
  "check four3" >:: (fun _ -> assert_equal (check_four update2 0) false);
  "check four3.1" >:: (fun _ -> assert_equal (check_four update2 1) false);
  "check legal3" >:: (fun _ -> assert_equal (check_legal update2 0) true);
  "check legal3.1" >:: (fun _ -> assert_equal (check_legal update2 1) true);

]

let suite = 
  "test suite for connectfour"  >::: List.flatten [
    parse_tests; 
    data_tests;
  ]

let _ = run_test_tt_main suite
