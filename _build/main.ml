open Move
open State
open Ai

(** [fst] takes in a tuple [tup] and returns the first entry in [tup]. *)
let fst tup =
  match tup with
  |(x, _) -> x

(** [snd] takes in a tuple [tup] and returns the second entry in [tup]. *)
let snd tup =
  match tup with
  |(_, y) -> y

(** [attack] is a recursive function that takes an [input] and a [state]. If
    the input is valid and can be parsed into an attack move, [attack] will have
    the player attack a location and the AI will respond with an attack of its
    own. After both players have attacked, [attack] will check to see if anyone
    has won. If no one has, it will prompt the user for another input.*)
let rec attack input state ai =
  try match (Move.parse input) with
    | exception End_of_file -> print_endline "Game ended. Goodbye!"
    | Move.Quit -> print_endline "Quitters never get anywhere! Goodbye loser!";
                   exit 0
    | Move.Place (ship, orient, pos) ->
      print_endline "Invalid command. You have already places all of your ships.";
      print_string  "> ";
      attack (read_line ()) state ai
    | Move.Attack (pos) ->
      let new_state = State.player_attack state pos in
      let new_tuple = Ai.attack ai new_state in
      let new_ai = snd new_tuple in
      let newest_state = fst new_tuple in
      print_endline "Your Board:";
      State.print_player newest_state;
      print_endline ("\nEnemy Board:");
      State.print_enemy newest_state;
      let temp = State.get_status state newest_state in
      if temp <> ""
      then print_endline ("You just sunk the " ^ temp ^ ".");
      if State.player_win newest_state
      then (print_string ("Congratulations " ^ (State.get_name newest_state));
            print_endline ("! You beat the AI!"); exit 0)
      else
        if State.ai_win newest_state
        then (print_string ("Sorry " ^ (State.get_name newest_state));
              print_endline (", looks like the AI beat you."); exit 0)
        else print_string "> ";
        attack (read_line()) newest_state new_ai
  with
  | Move.Empty ->
    print_endline "Please attack a position.";
    print_string  "> ";
    attack (read_line ()) state ai
  | Move.Illegal ->
    print_endline "Please type in a valid command. For example, to attack postition F7 on your oppenent's board, type \"attack F7\".";
    print_string  "> ";
    attack (read_line ()) state ai
  | State.InvalidAttackPos ->
    print_endline "Please type in a valid attack position. For example, to attack postition F7 on your oppenent's board, type \"attack F7\".";
    print_string  "> ";
    attack (read_line ()) state ai
  | State.InvalidPlacement ->
    print_endline "Please type in a valid attack position. For example, to attack postition F7 on your oppenent's board, type \"attack F7\".";
    print_string  "> ";
    attack (read_line ()) state ai

(** [place_ships] is a recursive function that takes an [input] and a [state].
    If the [input] is valid and can be parsed into a place move, [place_ships]
    will place the specified ship at the specified location with the given
    orientation. If all the ships are placed, [place_ships] will call [attack].
    Otherwise, if the [input] is not valid or there are still ships to be placed,
    then it will prompt the user to place another ship. *)
let rec place_ships input state ai =
  try match (Move.parse input) with
    | exception End_of_file -> print_endline "Game ended. Goodbye!"
    | Move.Quit -> print_endline "Quitters never get anywhere! Goodbye loser!";
                   exit 0
    | Move.Attack (pos) ->
      print_endline "Invalid command. Please place your ships first.";
      print_string  "> ";
      place_ships (read_line ()) state ai
    | Move.Place (ship, orient, pos) ->
      let result = State.player_place state ship orient pos in
      State.print_ships result;
      (* print_string ("  1 2 3 4 5 6 7 8 9\n" ^ print_grid2 result 'A' 0 ""); *)
      if State.remaining result = 0
      then (print_endline "Now that you have placed your ships, you can begin shooting. For example, to shoot B4, type \"attack B4\".";
            print_endline "An X represents a hit and an O represents a miss.";
            print_string "> ";
            attack (read_line()) result) ai
      else print_string"> ";
      place_ships (read_line()) result ai
  with
  | Move.Empty ->
    print_endline "Please place your ship.";
    print_string  "> ";
    place_ships (read_line ()) state ai
  | Move.Illegal ->
    print_endline "Please type in a valid command. For example, to place your submarine vertically at F7, type place submarine V F7.";
    print_string  "> ";
    place_ships (read_line ()) state ai
  | State.InvalidShipName ->
    print_endline "Please type in a valid ship. For reference, you have a carrier, battleship, cruiser, submarine, and destroyer.";
    print_string  "> ";
    place_ships (read_line ()) state ai
  | State.InvalidPlacement ->
    print_endline "Please type in a valid ship placement.";
    print_string  "> ";
    place_ships (read_line ()) state ai
  | State.InvalidOrientation ->
    print_endline "Please type in a valid orientation. H refers to horizonal and V refers to vertical placement.";
    print_string  "> ";
    place_ships (read_line ()) state ai

(** [read_level] takes in an [input] and checks to see if it is a valid level
    on the AI. Asks the player to input another level if it is invalid. *)
let rec read_level input =
  try Ai.init_ai input
  with
  | Ai.InvalidLevel ->
    print_endline "Please type in a valid level. You can choose beginner, amateur, professional, or master.";
    print_string "> ";
    read_level (read_line ())

(** [main ()] starts the game, asking for the player's name. *)
let main () =
  ANSITerminal.(print_string [blue]
                  "\n\nWelcome to Battleship!\n");
  print_endline "What is your name?";
  print_string  "> ";
  match read_line() with
  | exception End_of_file -> print_endline "Game ended. Goodbye!"
  | name -> print_endline ("Hello " ^ (String.trim name) ^ "!");
            print_endline ("First choose the level of AI you would like to play against: beginner, amateur, professional, or master.");
            print_string "> ";
    match read_line() with
    | exception End_of_file -> print_endline "Game ended. Goodbye!";
    | level -> let ai = read_level level in
      print_endline ("Great! You'll be playing the " ^ Ai.get_level ai ^ ".");
      print_endline ("Here's how you play: \nPlace your 5 ships on the grid (shown below). \nNo cheating - once you place your ships, they cannot be moved!");
      print_endline ("Here are your ships: carrier of length 5, battleship of length 4, cruiser of length 3, submarine of length 3, and destroyer of length 2.");
      print_endline ("You can decide whether to place a ship horizontally or vertically. Please make sure to give the position of the top left corner of the ship. \nFor example, to place the carrier horizontally starting at B4, you would type \"place carrier H B4\". ");
      State.print_player (State.init_state name);
      print_endline ("Let's get started!");
      print_string "> ";
      place_ships (read_line()) (State.init_state name) (ai)

(* Execute the game engine. *)
let () = main ()