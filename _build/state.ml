type position = {
  row: char;
  col: int;
}

type board = {
  ships_pos: (string * (position list)) list;
  hits: (string * (position list)) list;
  misses: position list;
  num_hits: int;
  not_sunken: string list;
}

type t = {
  name: string;
  player: board;
  enemy: board;
  avail: string list;
}

exception InvalidPlacement

exception InvalidAttackPos

exception InvalidShipName

exception InvalidOrientation

let rec list_rem elt lst acc =
  match lst with
  | [] -> acc
  | h::t ->
    if h = elt
    then acc@t
    else (list_rem elt t (h::acc))

(** [empty_board] takes in a unit and returns a completely empty board.*)
let empty_board () = {
  ships_pos = [];
  hits = [("carrier", []); ("battleship", []); ("cruiser", []);
          ("submarine", []); ("destroyer", [])];
  misses = [];
  num_hits = 0;
  not_sunken = ["carrier"; "battleship"; "cruiser"; "submarine"; "destroyer"];
}

(** [length_of_ship] takes in the name of a ship and returns its length. Raises
    InvalidShipName if the name of the ship does not exist. *)
let length_of_ship ship =
  match ship with
  | "carrier" -> 5
  | "battleship" -> 4
  | "cruiser" -> 3
  | "submarine" -> 3
  | "destroyer" -> 2
  | _ -> raise InvalidShipName

(** [fst] takes in a tuple and returns the first element. *)
let fst tup =
  match tup with (a, _) -> a

(** [snd] takes in a tuple and returns the second element. *)
let snd tup =
  match tup with (_, b) -> b

(** [next_pos] takes in a valid position and an orientation and returns the
    next location. Given an invalid orientation (i.e. not H or V), it raises
    the exception InvalidOrientation. *)
let next_pos (pos:position) (orient:string) =
  match orient with
  | "H" -> {row = pos.row; col = pos.col + 1}
  | "V" -> {row = Char.chr (Char.code pos.row + 1); col = pos.col}
  | _ -> raise InvalidOrientation

(** [in_bounds] takes in a position. If the position is on the grid, it returns
    true. Otherwise, it returns false. *)
let in_bounds pos =
  Char.code (pos.row) <= 73 && Char.code (pos.row) >= 65 && pos.col <= 9
  && pos.col >=1

(** [pos_in] is a helper function for [valid_place] that takes in a position
    and a list of ship positions. It returns a tuple with the first entry being
    a boolean that is true, if there is a ship at that position, and false
    otherwise. The second entry is the name of the ship. *)
let rec pos_in pos ships_pos =
  match ships_pos with
  | [] -> (false, "no ship")
  | (ship, pos_list)::t ->
    if List.mem pos pos_list
    then (true, ship)
    else pos_in pos t

(** [valid_place] is a helper function for [place_ship] that takes in a board,
    a ship name, an orientation, and a starting position and returns true if
    the entire ship can be placed on the grid without overlapping and false
    otherwise. *)
let valid_place board ship orient pos =
  let pos_empty pos = not (fst (pos_in pos board.ships_pos)) in
  let rec check_neighbors n orient pos =
    match n with
    | 0 -> true
    | x ->
      if in_bounds pos && pos_empty pos
      then check_neighbors (x-1) orient (next_pos pos orient)
      else false in
  check_neighbors (length_of_ship ship) orient pos

(** [place_ship] takes in a board, a ship name, an orientation, and a starting
    position then returns a new board with the ship placed on it. Raises
    InvalidPlacement if the ship cannot be placed on the board. *)
let place_ship board ship orient pos =
  let rec get_neighbors n orient pos (acc:position list) : position list =
    match n with
    | 1 -> acc
    | n -> get_neighbors (n-1) orient (next_pos pos orient)
             ((next_pos pos orient)::acc) in
  {
    ships_pos =
      if valid_place board ship orient pos
      then board.ships_pos@[(ship, get_neighbors
                               (length_of_ship ship) orient pos [pos])]
      else raise InvalidPlacement;
    hits = board.hits;
    misses = board.misses;
    num_hits = board.num_hits;
    not_sunken = board.not_sunken;
  }

(** [helper] takes in a string representing a position and turns it into a
    position. Raises InvalidPlacement if the position is incorrect. *)
let helper pos =
  try match (int_of_string (String.sub pos 1 1)) with
    | _ -> let row_val = String.get pos 0 in
      let col_val = int_of_string (String.sub pos 1 ((String.length pos)-1)) in
      {
        row = row_val;
        col = col_val;
      }
  with
  |Failure (arg) -> raise InvalidPlacement
  |Invalid_argument (arg) -> raise InvalidPlacement

(** [player_place] takes in a state, a ship, an orientation, and a starting
    orientation. Returns a new state with the ship placed on the player's board.
    Raises InvalidShipName if the name of the ship does not exist. *)
let player_place state ship orient pos =
  if List.mem ship (state.avail)
  then
    {
      name = state.name;
      player = place_ship state.player ship orient (helper pos);
      enemy = state.enemy;
      avail = list_rem ship (state.avail) [];
    }
  else raise InvalidShipName

(** [remaining] takes in a state and returns however many ships still need to
    be placed. If there are two or more ships left, it simply returns 2. *)
let rec remaining state =
  match state.avail with
  | [] -> 0
  | h::[] -> 1
  | _ -> 2

(** [rand_pos] generates a random position. *)
let rand_pos () = {
  row = (Char.chr (Random.int 9 + 65));
  col = (Random.int 9 + 1);
}

(** [random_board] generates a random board with random ship placements. *)
let random_board () =
  let rand_ori () =
    if Random.int 2 = 0
    then "H"
    else "V" in
  let rec place_ships_rand board ships =
    match ships with
    | [] -> board
    | h::t ->
      try place_ships_rand (place_ship board h (rand_ori ()) (rand_pos ())) t
      with
      | InvalidPlacement -> place_ships_rand board ships in
  place_ships_rand (empty_board ()) ["carrier"; "battleship"; "cruiser";
                                     "submarine"; "destroyer"]

(** [init_state] takes in a str and creates an initial state with the string
    as the player's name, an empty player board, a random enemy board, and all
    the ships being available. *)
let init_state str =
  Random.self_init();
  {
    name = str;
    player = empty_board ();
    enemy = random_board ();
    avail = ["carrier"; "battleship"; "cruiser"; "submarine"; "destroyer"]
  }

(** [remove] takes in a ship and list of ship positions and removes it from the
    list. *)
let remove ship ships_pos =
  List.remove_assoc ship ships_pos

(** [add_hit] takes in a position, the name of the ship, the list of hits, and
    an accumulator, then adds the new hit to the list of hits. Fails if the
    hit list is invalid (does not contain all the ships).*)
let rec add_hit pos shipname hits acc =
  match hits with
  | (ship, lst)::t ->
    if ship = shipname
    then
      if List.mem pos lst
      then hits
      else (ship, pos::lst)::(remove ship hits)@acc
    else add_hit pos shipname t ((ship, lst)::acc)
  | _ -> failwith "invalid hits list"

(** [not_attacked] takes in a position, the list of misses, and the list of
    hits. It returns true if the position has not been attacked and false
    otherwise. *)
let not_attacked pos misses hits =
  let rec not_hit pos hits =
    match hits with
    | [] -> true
    | (ship, lst)::t ->
      if List.mem pos lst
      then false
      else not_hit pos t
  in not_hit pos hits && not (List.mem pos misses)

(** [sunk_ship] takes in the name of a ship and a list of all the hits. Returns
    true if the given ship is sunken and false otherwise. *)
let sunk_ship ship hits =
  match hits with
  | [] -> false
  | (name, pos_lst)::t ->
    if name = ship
    then List.length pos_lst = length_of_ship ship
    else false

(** [get_sunken_ship] takes in a list of all the hits ashnd a list of ships that
    have not sunk yet. It returns an empty string if none of the ships in that
    list have sunk, otherwise it returns the name of the ship that was sunk. *)
let rec get_sunken_ship hits lst =
  match lst with
  | [] -> ""
  | h::t ->
    if sunk_ship h hits
    then h
    else get_sunken_ship hits t

(** [attack] takes in a state, a board, and the position being attacked.
    Returns a new board which either adds to the misses, the hits, or removes a
    ship from the list of not sunken ships. Returns InvalidAttackPos if the
    position is not on the grid or has already been attacked. *)
let attack state board pos =
  if in_bounds pos && not_attacked pos board.misses board.hits
  then
    if fst (pos_in pos board.ships_pos) = true
    then let new_hits = add_hit pos (snd (pos_in pos board.ships_pos))
             board.hits [] in
      (
        {
          ships_pos = board.ships_pos;
          hits = new_hits;
          misses = board.misses;
          num_hits = board.num_hits + 1;
          not_sunken = (list_rem (get_sunken_ship new_hits board.not_sunken)
                          (board.not_sunken) []);
        }
      )
    else
      (
        {
          ships_pos = board.ships_pos;
          hits = board.hits;
          misses = if List.mem pos board.misses then board.misses
            else pos::board.misses;
          num_hits = board.num_hits;
          not_sunken = board.not_sunken;
        }
      )
  else raise InvalidAttackPos

(** [player_attack] takes in a state and a pos and returns a new state that has
    called [attack] on the enemy's board. *)
let player_attack state pos =
  {
    name = state.name;
    player = state.player;
    enemy = attack state state.enemy (helper pos);
    avail = state.avail;
  }

(** [ai_attack] takes in a state and a pos and returns a new state that has
    called [attack] on the player's board. *)
let rec ai_attack state pos =
  try
    {
      name = state.name;
      player = attack state state.player (pos);
      enemy = state.enemy;
      avail = state.avail;
    }
  with InvalidAttackPos -> failwith "Not a valid position"

(** [random_ai_attack] takes in a state and a pos and returns a new state that
    has called [attack] on the player's board. *)
let rec random_ai_attack state =
  try
    let random_pos = rand_pos () in
    ({
      name = state.name;
      player = attack state state.player random_pos;
      enemy = state.enemy;
      avail = state.avail;
    }, random_pos)
  with InvalidAttackPos -> random_ai_attack state

(** [search_hits] takes in a list of hits and a pos. Returns true if the pos is
    a hit and false otherwise. *)
let rec search_hits hits pos =
  match hits with
  | [] -> false
  | (k,v)::t ->
    if List.mem pos v
    then true
    else search_hits t pos

(** [is_sunken_position] takes in a [board] and a [pos]. Returns true if the
    given [pos] contains a sunken ship and false otherwise. *)
let is_sunken_position board pos =
  match pos_in pos board.ships_pos with
  |(false, _) -> false
  |(true, ship_name) -> not (List.mem ship_name (board.not_sunken))

(** [return_sign] takes in a board and a string representing a position. Returns
    a string associated with the position. O for a miss, X for a hit, and + if
    the location has not been attacked. *)
let return_sign board str =
  let hits = board.hits in
  let misses = board.misses in
  let pos = helper str in
  if List.mem pos misses
  then "O"
  else
  if search_hits hits pos
  then "X"
  else "+"

(** [return_ship_grid] takes in a [board], a [row], and a [col]. Returns a
    string associated with the position. > if a ship is placed there
    and + if a ship is not. *)
let return_ship_grid board row col =
  let tup = pos_in (helper ((String.make 1 (Char.chr (Char.code row))) ^
                            (string_of_int col))) board.ships_pos in
  if fst tup
  then (">", [ANSITerminal.Bold; ANSITerminal.blue])
  else ("+", [])

(** [return_sign2] takes in a [board], a [row], a [col], and a [flag]. Returns
    a string associated with the position [row] [col]. O if it is a miss, X if
    is a hit, > if a ship is placed there and the flag is true, and +
    otherwise. *)
let return_sign2 board row col flag =
  let hits = board.hits in
  let misses = board.misses in
  let pos = helper ((String.make 1 (Char.chr (Char.code row))) ^ (
      string_of_int col)) in
  if List.mem pos misses
  then ("O", [ANSITerminal.Bold; ANSITerminal.white])
  else
  if search_hits hits pos
  then (if is_sunken_position board pos
        then ("X", [ANSITerminal.Bold; ANSITerminal.red; ANSITerminal.on_red])
        else ("X", [ANSITerminal.Bold; ANSITerminal.red]))
  else (if flag
        then return_ship_grid board row col
        else ("+", []))

(** [gen_grid] is a recursive function that takes in a [board], [row], [col],
    and [acc] and returns a list of rows (represented by a list) containing
    the string representation of each position along with its ANSITerminal
    color. *)
let rec gen_grid board row col acc flag =
  let rec gen_row row col row_acc =
    match col with
    | 0 -> (((String.make 1 row), [ANSITerminal.Bold])::row_acc)
    | n -> gen_row row (col-1) ((return_sign2 board row col flag)::row_acc)
  in
  match row with
  | '@' -> acc
  | n -> gen_grid board (Char.chr (Char.code n - 1)) 9 ((gen_row n 9 [])::acc) flag

let rec gen_grid2 board row col acc =
  let rec gen_row row col row_acc =
    match col with
    | 0 -> (((String.make 1 row), [ANSITerminal.Bold])::row_acc)
    | n -> gen_row row (col-1) ((return_ship_grid board row col)::row_acc)
  in
  match row with
  | '@' -> acc
  | n -> gen_grid2 board (Char.chr (Char.code n - 1)) 9 ((gen_row n 9 [])::acc)

(** [prettify] takes in a list [all_pos] and an [acc] and adds in spaces
    between the positions. *)
let rec prettify all_pos acc =
  match all_pos with
  | [] -> acc
  | row::t ->
    let rec prettify_row row_lst =
      match row_lst with
      | (char1, colors1)::(char2, colors2)::t ->
        if List.mem ANSITerminal.on_red colors1 && List.mem
             ANSITerminal.on_red colors2
        then (char1, colors1)::(" ",
                                [ANSITerminal.on_red])::(prettify_row
                                                           ((char2, colors2)::t))
        else (char1, colors1)::(" ", [])::(prettify_row ((char2, colors2)::t))
      | h::[] -> h::[]
      | [] -> [] in
    prettify t acc@[prettify_row row]

(** [pretty_print] takes in a list [all_pos_lst] and prints out the string
    representation of each position using ANSITerminal. *)
let rec pretty_print all_pos_lst =
  let rec pretty_print_row row_lst =
    match row_lst with
    | [] -> ANSITerminal.(print_string [] ("\n"));
    | row::t -> ANSITerminal.(print_string (snd row) (fst row));
      pretty_print_row t in
  let printable = prettify all_pos_lst [] in
  let rec printing lst =
    match lst with
    | [] -> ANSITerminal.(print_string [] (""));
    |h::t -> pretty_print_row h; printing t in
  printing printable

(** [print] takes in a [board] and pretty prints it. *)
let print board flag =
  pretty_print (List.rev ([("  1 2 3 4 5 6 7 8 9",
                        [ANSITerminal.Bold])]::(gen_grid board 'I' 9 [] flag)))

(** [print_enemy] takes in a [state] and prints the enemy's board. *)
let print_enemy state =
  print state.enemy false

(** [print_player] takes in a [state] and prints the player's board. *)
let print_player state =
  print state.player true

let print_ships state =
  pretty_print (List.rev ([("  1 2 3 4 5 6 7 8 9",
                    [ANSITerminal.Bold])]::(gen_grid2 state.player 'I' 9 [])))

(** [get_status] takes in a [state] and calls [get_sunken_ship]. Returns an empty
    string if no ship has just been sunk by the player, otherwise returns the
    name of the ship that was sunk. *)
let get_status old_state new_state =
  get_sunken_ship new_state.enemy.hits old_state.enemy.not_sunken

(** [get_player_status] takes in a [state] and calls [get_sunken_ship]. Returns
    an empty string if no ship has just been sunk by the AI, otherwise returns
    the name of the ship that was sunk. *)
let get_player_status old_state new_state =
  get_sunken_ship new_state.player.hits old_state.player.not_sunken

(** [get_player_sign] takes in a [state] and a string representing a position.
    Calls [return_sign] and returns a string associated with the position on
    the player's board. *)
let get_player_sign state str =
  return_sign state.player str

(** [get_enemy_sign] takes in a [state] and a string representing a position.
    Calls [return_sign] and returns a string associated with the position on
    the enemy's board. *)
let get_enemy_sign state str =
  return_sign state.enemy str

(** [player_win] takes in a [state] and returns true if the player has won and
    false otherwise. *)
let player_win state =
  state.enemy.num_hits = 17

(** [ai_win] takes in a [state] and returns true if the AI has won and false
    otherwise. *)
let ai_win state =
  state.player.num_hits = 17

(** [get_name] takes in a [state] and returns the name of the player. *)
let get_name state =
  state.name

(** [get_not_sunkent] takes in a [state] and returns the list of not sunken ships
    on the player's board. *)
let get_not_sunken state =
  state.player.not_sunken

(** [get_enemy_hits] takes in a [state] and returns the ai's number of hits. *)
let get_enemy_hits state =
  state.player.num_hits

(** [get_enemy_attacks] returns a list of all positions on the player's board
    attacked by the enemy. *)
let get_enemy_attacks state =
  let rec get_hits lst acc =
    match lst with
    | [] -> acc
    | (ship, hit_lst)::t ->
      get_hits t acc@hit_lst in
  let hits = get_hits (state.player.hits) [] in
  state.player.misses@hits

(** [get_ship_pos] takes in a [state] and returns the player's placement of
    ships. *)
let get_ship_pos state =
  state.player.ships_pos

(** [ship_positions] takes in a [state] and returns a list of positions of all
    the ships. *)
let ship_positions state =
  let rec get_pos lst acc =
    match lst with
    | [] -> acc
    | (_, pos_lst)::t -> get_pos t (pos_lst@acc) in
  get_pos (state.player.ships_pos) []