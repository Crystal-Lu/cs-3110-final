open Graphics
open Data

type box = {x : int;
            y : int;
            w : int;
            h : int;}

let draw_box box =
  Graphics.moveto box.x box.y;
  Graphics.lineto (box.x+box.w) box.y;
  Graphics.lineto (box.x+box.w) (box.y+box.h);
  Graphics.lineto box.x (box.y+box.h);
  Graphics.lineto box.x box.y

let initial_gui = 
  [{x=0;y=0;w=50;h=50};{x=52;y=0;w=50;h=50};{x=104;y=0;w=50;h=50};
   {x=156;y=0;w=50;h=50};{x=208;y=0;w=50;h=50};{x=260;y=0;w=50;h=50};
   {x=312;y=0;w=50;h=50};{x=0;y=52;w=50;h=50};{x=52;y=52;w=50;h=50};
   {x=104;y=52;w=50;h=50};{x=156;y=52;w=50;h=50};{x=208;y=52;w=50;h=50};
   {x=260;y=52;w=50;h=50};{x=312;y=52;w=50;h=50};{x=0;y=104;w=50;h=50};
   {x=52;y=104;w=50;h=50};{x=104;y=104;w=50;h=50};{x=156;y=104;w=50;h=50};
   {x=208;y=104;w=50;h=50};{x=260;y=104;w=50;h=50};{x=312;y=104;w=50;h=50};
   {x=0;y=156;w=50;h=50};{x=52;y=156;w=50;h=50};{x=104;y=156;w=50;h=50};
   {x=156;y=156;w=50;h=50};{x=208;y=156;w=50;h=50};{x=260;y=156;w=50;h=50};
   {x=312;y=156;w=50;h=50};{x=0;y=208;w=50;h=50};{x=52;y=208;w=50;h=50};
   {x=104;y=208;w=50;h=50};{x=156;y=208;w=50;h=50};{x=208;y=208;w=50;h=50};
   {x=260;y=208;w=50;h=50};{x=312;y=208;w=50;h=50};{x=0;y=260;w=50;h=50};
   {x=52;y=260;w=50;h=50};{x=104;y=260;w=50;h=50};{x=156;y=260;w=50;h=50};
   {x=208;y=260;w=50;h=50};{x=260;y=260;w=50;h=50};{x=312;y=260;w=50;h=50};]

let draw_initial =
  Graphics.close_graph ();
  Graphics.open_graph " 364x312";
  List.iter draw_box initial_gui

let draw_board state move =
  try
    let row = Data.empty_spot state move 0 in
    match (Data.current_player state) with
    | A -> (
        Graphics.moveto (52*move + 5) (52*(row-1) + 5);
        Graphics.draw_char 'B';
      )
    | B -> (
        Graphics.moveto (52*move + 5) (52*(row-1) + 5);
        Graphics.draw_char 'A';
      )
  with
    Full -> match (Data.current_player state) with
    | A -> (
        Graphics.moveto (52*move + 5) (265);
        Graphics.draw_char 'B';
      )
    | B -> (
        Graphics.moveto (52*move + 5) (265);
        Graphics.draw_char 'A';
      )