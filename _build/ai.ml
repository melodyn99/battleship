open State

type pos_info = {
  priority:int;
  attack:bool;
}

type position = State.position

type t = {
  last_hit: string;
  last_orientation:string;
  board: (position * pos_info) list;
  level:string;
  on_hunt:bool;
  unresolved_hits: position list;
  target_misses: position list;
  next_hit: string;
  original_hit: string;
  direction: int;
}

exception InvalidLevel

let init_board =
  [(State.helper("A1"), {priority = 0; attack = false});
   (State.helper("A2"), {priority = 0; attack = false});
   (State.helper("A3"), {priority = 0; attack = false});
   (State.helper("A4"), {priority = 0; attack = false});
   (State.helper("A5"), {priority = 0; attack = false});
   (State.helper("A6"), {priority = 0; attack = false});
   (State.helper("A7"), {priority = 0; attack = false});
   (State.helper("A8"), {priority = 0; attack = false});
   (State.helper("A9"), {priority = 0; attack = false});

   (State.helper("B1"), {priority = 0; attack = false});
   (State.helper("B2"), {priority = 0; attack = false});
   (State.helper("B3"), {priority = 0; attack = false});
   (State.helper("B4"), {priority = 0; attack = false});
   (State.helper("B5"), {priority = 0; attack = false});
   (State.helper("B6"), {priority = 0; attack = false});
   (State.helper("B7"), {priority = 0; attack = false});
   (State.helper("B8"), {priority = 0; attack = false});
   (State.helper("B9"), {priority = 0; attack = false});

   (State.helper("C1"), {priority = 0; attack = false});
   (State.helper("C2"), {priority = 0; attack = false});
   (State.helper("C3"), {priority = 0; attack = false});
   (State.helper("C4"), {priority = 0; attack = false});
   (State.helper("C5"), {priority = 0; attack = false});
   (State.helper("C6"), {priority = 0; attack = false});
   (State.helper("C7"), {priority = 0; attack = false});
   (State.helper("C8"), {priority = 0; attack = false});
   (State.helper("C9"), {priority = 0; attack = false});

   (State.helper("D1"), {priority = 0; attack = false});
   (State.helper("D2"), {priority = 0; attack = false});
   (State.helper("D3"), {priority = 0; attack = false});
   (State.helper("D4"), {priority = 0; attack = false});
   (State.helper("D5"), {priority = 0; attack = false});
   (State.helper("D6"), {priority = 0; attack = false});
   (State.helper("D7"), {priority = 0; attack = false});
   (State.helper("D8"), {priority = 0; attack = false});
   (State.helper("D9"), {priority = 0; attack = false});

   (State.helper("E1"), {priority = 0; attack = false});
   (State.helper("E2"), {priority = 0; attack = false});
   (State.helper("E3"), {priority = 0; attack = false});
   (State.helper("E4"), {priority = 0; attack = false});
   (State.helper("E5"), {priority = 0; attack = false});
   (State.helper("E6"), {priority = 0; attack = false});
   (State.helper("E7"), {priority = 0; attack = false});
   (State.helper("E8"), {priority = 0; attack = false});
   (State.helper("E9"), {priority = 0; attack = false});

   (State.helper("F1"), {priority = 0; attack = false});
   (State.helper("F2"), {priority = 0; attack = false});
   (State.helper("F3"), {priority = 0; attack = false});
   (State.helper("F4"), {priority = 0; attack = false});
   (State.helper("F5"), {priority = 0; attack = false});
   (State.helper("F6"), {priority = 0; attack = false});
   (State.helper("F7"), {priority = 0; attack = false});
   (State.helper("F8"), {priority = 0; attack = false});
   (State.helper("F9"), {priority = 0; attack = false});

   (State.helper("G1"), {priority = 0; attack = false});
   (State.helper("G2"), {priority = 0; attack = false});
   (State.helper("G3"), {priority = 0; attack = false});
   (State.helper("G4"), {priority = 0; attack = false});
   (State.helper("G5"), {priority = 0; attack = false});
   (State.helper("G6"), {priority = 0; attack = false});
   (State.helper("G7"), {priority = 0; attack = false});
   (State.helper("G8"), {priority = 0; attack = false});
   (State.helper("G9"), {priority = 0; attack = false});

   (State.helper("H1"), {priority = 0; attack = false});
   (State.helper("H2"), {priority = 0; attack = false});
   (State.helper("H3"), {priority = 0; attack = false});
   (State.helper("H4"), {priority = 0; attack = false});
   (State.helper("H5"), {priority = 0; attack = false});
   (State.helper("H6"), {priority = 0; attack = false});
   (State.helper("H7"), {priority = 0; attack = false});
   (State.helper("H8"), {priority = 0; attack = false});
   (State.helper("H9"), {priority = 0; attack = false});

   (State.helper("I1"), {priority = 0; attack = false});
   (State.helper("I2"), {priority = 0; attack = false});
   (State.helper("I3"), {priority = 0; attack = false});
   (State.helper("I4"), {priority = 0; attack = false});
   (State.helper("I5"), {priority = 0; attack = false});
   (State.helper("I6"), {priority = 0; attack = false});
   (State.helper("I7"), {priority = 0; attack = false});
   (State.helper("I8"), {priority = 0; attack = false});
   (State.helper("I9"), {priority = 0; attack = false})]




let diagonal = [{row = 'A'; col = 1}; {row = 'B'; col = 2}; {row = 'C'; col = 3};
                {row = 'D'; col = 4}; {row = 'E'; col = 5}; {row = 'F'; col = 6};
                {row = 'G'; col = 7}; {row = 'H'; col = 8}; {row = 'I'; col = 9}]

let string_pos = ["A1";"A2";"A3";"A4";"A5";"A6";"A7";"A8";"A9";
                  "B1";"B2";"B3";"B4";"B5";"B6";"B7";"B8";"B9";
                  "C1";"C2";"C3";"C4";"C5";"C6";"C7";"C8";"C9";
                  "D1";"D2";"D3";"D4";"D5";"D6";"D7";"D8";"D9";
                  "E1";"E2";"E3";"E4";"E5";"E6";"E7";"E8";"E9";
                  "F1";"F2";"F3";"F4";"F5";"F6";"F7";"F8";"F9";
                  "G1";"G2";"G3";"G4";"G5";"G6";"G7";"G8";"G9";
                  "H1";"H2";"H3";"H4";"H5";"H6";"H7";"H8";"H9";
                  "I1";"I2";"I3";"I4";"I5";"I6";"I7";"I8";"I9";]

(** [get_shortest] takes in a state and returns the length of the shortest ship
    that has not yet been sunken on the player's board. *)
let get_shortest state =
  let lst = State.get_not_sunken state in
  let rec find_min lst min =
    match lst with
    | [] -> min
    | h::t -> let length = State.length_of_ship h in
      if length < min then find_min t length else find_min t min in
  find_min lst 5

(** [get_list] takes in a [lst], an [acc], and [h_val] and returns a new list
    of positions with the highest values. *)
let rec get_list lst acc h_val =
  match lst with
  | [] -> acc
  | (pos, info)::t -> let value = info.priority in
    if value > h_val && not (info.attack) then get_list t (pos::[]) value
    else if value = h_val && not (info.attack) then get_list t (pos::acc) value
    else get_list t acc h_val

(** [choose] takes in a [lst], a [num] which is a random int, and an [acc].
    Returns an element from the [lst]. *)
let rec choose lst num acc =
  match lst with
  |[] -> failwith "out of bounds"
  |h::t -> if num = acc then h else choose t num (acc+1)

(** [get_highest_value] takes in a [lst] whcih represents a board and returns
    the highest or one of the highest values found in the board. *)
let get_highest_value lst =
  let high_lst = get_list lst [] (-100) in
  choose high_lst (Random.int (List.length high_lst)) 0

(** [h_offset] takes in a [pos] and an int [n]. Returns a position horizontally
    offset by [n] from [pos]. A negative value for [n] indicates moving to the
    left of [pos]. *)
let h_offset pos n =
  {
    row = pos.row;
    col = pos.col + n;
  }

(** [v_offset] takes in a [pos] and an int [n]. Returns a position vertically
    offset by [n] from [pos]. A negative value for [n] indicates moving up from
    [pos]. *)
let v_offset pos n =
  {
    row = Char.chr (Char.code (pos.row) + n);
    col = pos.col;
  }

(* (** [add_row_offset] takes in a [pos], an [acc]*)
   let rec add_row_offset pos acc n =
   let next = h_offset pos n in
   if State.in_bounds next
   then add_row_offset next (next::acc) n
   else acc

 * [update_diagonals] takes in a list containing the main diagonal positions,
    the offset [n], and an [acc]. Returns a list of positions that have even
    parity.
   let rec update_diagonals lst n acc =
   match lst with
   | [] -> acc
   | h::t ->
    let to_left = add_row_offset h [] (-n) in
    let to_right = add_row_offset h [] n in
    let row_evens = to_left@[h]@to_right in
    update_diagonals t n (acc@row_evens)
*)

(** [get_info] takes in a [pos] and a [board] and returns the position info
    associated with the [pos]. *)
let rec get_info pos board =
  match board with
  |[] -> failwith "no pos"
  | (k, v)::t -> if k = pos then v else get_info pos t

(** [h_offset_max] returns the most spaces that can be traversed in [dir] less
    than a maximum [len]. [dir] is -1 (left) or 1 (right). *)
let rec h_offset_max pos dir len board acc =
  let next = h_offset pos dir in
  if (State.in_bounds next && not ((get_info pos board).attack)) && acc < (len-1)
  then h_offset_max next dir len board (acc+1)
  else acc

(** [v_offset_max] returns the most spaces that can be traversed in [dir] less
    than a maximum [len]. [dir] is -1 (up) or 1 (down). *)
let rec v_offset_max pos dir len board acc =
  let next = v_offset pos dir in
  if (State.in_bounds next && not ((get_info pos board).attack)) && acc < (len-1)
  then v_offset_max next dir len board (acc+1)
  else acc

(** [count_permutations] takes in a [pos], a list of not sunken [ships], the
    [ai], and an [acc]. Returns the number of possible placements of all the
    not sunken ships. Calls [h_offset_max] and [v_offset_max] to determine the
    placements horizontally and vertically. *)
let rec count_possibilities pos ships ai acc =
  match ships with
  | [] -> acc
  | h::t -> let length = State.length_of_ship h in
    let left_offset = h_offset_max pos (-1) length (ai.board) 0 in
    let right_offset = h_offset_max pos 1 length (ai.board) 0 in
    let width = right_offset + left_offset + 1 in
    let up_offset = v_offset_max pos (-1) length (ai.board) 0 in
    let down_offset = v_offset_max pos 1 length (ai.board) 0 in
    let height = up_offset + down_offset + 1 in
    let h_possibilities = max 0 (width - length + 1) in
    let v_possibilities = max 0 (height - length + 1) in
    count_possibilities pos t ai (acc + h_possibilities + v_possibilities)

(** [is_attacked] takes in a [board] and [pos] and returns whether the
    position has been attack or not. *)
let rec is_attacked board pos =
  match board with
  | [] -> false
  | (k, v)::t -> if k = pos then v.attack
    else is_attacked t pos

(** [generate_board] takes in a [state], an [ai], a [pos_lst], and an [acc].
    Returns a new list containing a tuple of positions and position infos where
    the priority is the number of possible orientations of each available ship
    that the player could place on the board. *)
let rec generate_board state ai pos_lst acc =
  match pos_lst with
  | [] -> acc
  | h::t -> let pos = State.helper h in
        generate_board state ai t
            ((pos, {priority = (count_possibilities pos
                                      (State.get_not_sunken state) ai 0);
                    attack = (is_attacked ai.board pos)})::acc)

(** [parse_level] takes in a [str] and pattern matches it to a level. If it
    doesn't match any levels, raises InvalidLevel. *)
let parse_level str =
  match str with
  |"beginner" -> "beginner"
  |"amateur" -> "amateur"
  |"professional" -> "professional"
  |"master" -> "master"
  | _ -> raise InvalidLevel

(** [init_ai] takes in a [str] representing the level of the ai. Returns a new
    ai with everything set to default values. *)
let init_ai str =
  {
    last_hit = "";
    last_orientation = "";
    board = init_board;
    level = parse_level (String.lowercase_ascii (String.trim str));
    on_hunt = true;
    unresolved_hits = [];
    target_misses = [];
    next_hit = "";
    original_hit = "";
    direction = 0;
  }

(** [gen_rand_pos] generates a valid random position, where valid means the
    position is in bounds and has not been attacked. *)
let rec gen_rand_pos board =
  let pos = State.rand_pos () in
  if not (is_attacked board pos) then pos
  else gen_rand_pos board

(** [valid_horizontal] takes in a [pos] and a [board] and returns the possible
    horizontal positions adjacent to the given [pos]. *)
let valid_horizontal pos board =
  let pos1 = ({row = pos.row; col = pos.col - 1}, "H") in
  let pos2 = ({row = pos.row; col = pos.col + 1}, "H") in
  let rec add_pos lst acc =
    match lst with
    |[] -> acc
    | (h, x)::t ->
      if State.in_bounds h && (not (is_attacked board h))
      then add_pos t ((h,x)::acc)
      else add_pos t acc in
  add_pos (pos1::pos2::[]) []

(** [valid_vertical] takes in a [pos] and a [board] and returns the possible
    vertical positions adjacent to the given [pos]. *)
let valid_vertical pos board =
  let pos1 = ({row = Char.chr ((Char.code pos.row) + 1); col = pos.col}, "V") in
  let pos2 = ({row = Char.chr ((Char.code pos.row) - 1); col = pos.col}, "V") in
  let rec add_pos lst acc =
    match lst with
    |[] -> acc
    | (h, x)::t ->
      if State.in_bounds h && (not (is_attacked board h))
      then add_pos t ((h,x)::acc)
      else add_pos t acc in
  add_pos (pos1::pos2::[]) []

(** [valid_adj] takes in a [pos], a [board], and ann [orient] then returns all
    valid adjacent positions to the [pos] based on the given [orient]. *)
let valid_adj pos board orient =
  match orient with
  | "" -> (valid_vertical pos board)@(valid_horizontal pos board)
  | "H" -> valid_horizontal pos board
  | _ -> valid_vertical pos board

(** [pos_to_string] takes in a [pos] and returns a string representation. *)
let pos_to_string pos =
  (String.make 1 (pos.row) ^ (string_of_int pos.col))

(** [is_hit] takes in a [pos] and a [state] and returns true if the [pos] is a
    hit and false otherwise. *)
let is_hit pos state =
  let lst = State.ship_positions state in
  List.mem pos lst

(** [switch_orient] takes in an [orient] and returns the opposite orientation. *)
let switch_orient orient =
  match orient with
  |"H" -> "V"
  |"V" -> "H"
  |_ -> ""

(** [target] takes in an [ai] and returns the [pos] of the next hit. *)
let target ai = (* only hits at the four points adjacent to the original hit, need to update it to move from last_hit *)
  try
    if ai.last_hit = ""
    then let valid_adj_lst = valid_adj (helper ai.original_hit) ai.board "" in
      choose valid_adj_lst (Random.int (List.length valid_adj_lst)) 0
    else (let valid_adj_lst = valid_adj (helper ai.last_hit) ai.board
                                                          ai.last_orientation in
          if valid_adj_lst = []
          then (let valid_adj_lst = valid_adj (helper ai.original_hit) ai.board
                                                          ai.last_orientation in
                if valid_adj_lst = []
                then (let valid_adj_lst = valid_adj (helper ai.original_hit)
                                ai.board (switch_orient ai.last_orientation) in
                      choose valid_adj_lst (Random.int (List.length valid_adj_lst)) 0)
                else choose valid_adj_lst (Random.int (List.length valid_adj_lst)) 0)
          else choose valid_adj_lst (Random.int (List.length valid_adj_lst)) 0)
  with
  | Invalid_argument (arg) -> (gen_rand_pos ai.board, "")

(** [get_info] takes in a [board] and a [pos] and returns the information
    (i.e. priority and attack) associated with the position. *)
let rec get_info board pos =
  match board with
  |[] -> failwith "nonexistent position"
  | (p, info)::t -> if p = pos then info else get_info t pos

(** [remove] takes in a [board], a [pos], and an [acc]. Returns a new board
    with the given position removed. *)
let rec remove board pos acc =
  match board with
  |[] -> acc
  | (p, info)::t -> if p = pos then acc@t else remove t pos ((p, info)::acc)

(** [update_board] takes in a [board] and a [pos]. Returns a new board that
    replaces the old position with a new position with attack set to true.
    Called when the AI attacks, so the board can be updated. *)
let update_board board pos =
  let info = get_info board pos in
  let new_board = remove board pos [] in
  ((pos, {priority = info.priority; attack = true})::new_board)

(** [update_ai] takes in an [ai], a [pos], a boolean [hunt], a list
    [unresolved], an [orig_hit], a [l_hit], and an [orient]. Returns a new ai
    with the updated parameters. *)
let update_ai ai pos hunt unresolved orig_hit l_hit orient =
  {
    last_hit = l_hit;
    last_orientation = orient;
    board = update_board (ai.board) pos;
    level = ai.level;
    on_hunt = hunt;
    unresolved_hits = unresolved;
    target_misses = ai.target_misses;
    next_hit = ai.next_hit;
    original_hit = orig_hit;
    direction = ai.direction;
  }

(** [find_ship_pos] takes in a [ship] and a list of [ship_pos], which is an
    association list of the ship name and the list of positions. Returns the
    list containing only the positions of the ship. *)
let rec find_ship_pos ship ship_pos=
  match ship_pos with
  |[] -> failwith "no ship"
  | (name, lst)::t -> if name = ship then lst else find_ship_pos ship t

(** [remove_pos] takes in a [pos], an [unresolved_lst] which is a list of
    positions containing all the unresolved hits, and an [acc]. Returns a new
    list with the [pos] removed from [unresolved_lst]. *)
let rec remove_pos pos unresolved_lst acc =
  match unresolved_lst with
  |[] -> acc
  |h::t -> if h = pos then remove_pos pos t acc else remove_pos pos t (h::acc)

(** [create_ship_pos] takes in a [ship_pos_lst] and an [unresolved_lst]. Returns
    a new list with all the positions found in [ship_pos_lst] removed from the
    [unresolved_lst]. *)
let rec create_ship_pos ship_pos_lst unresolved_lst =
  match ship_pos_lst with
  |[] -> unresolved_lst
  |h::t -> create_ship_pos t (remove_pos h unresolved_lst [])

(** [remove_sunk_ships] takes in a [new_state], [unresolved_hits], and a
    [ship_name]. Returns a new list containing all the unresolved hits with the
    positions the [ship_name] is in removed. *)
let remove_sunk_ships new_state unresolved_hits ship_name =
  let ship_pos = State.get_ship_pos new_state in
  if ship_name <> ""
  then (let ship_pos_lst = find_ship_pos ship_name ship_pos in
        create_ship_pos ship_pos_lst unresolved_hits)
  else unresolved_hits

(** [easy_attack] takes in a [state] and an [ai]. Returns a tuple containing
    the new state and new ai. Easiest level of the AI - the AI guesses random
    positions, regardles of hits or misses. *)
let easy_attack state ai =
  (fst (State.random_ai_attack state), ai)

(** [normal_attack] takes in a [state] and an [ai]. Returns a tuple containing
    the new state and new ai. Normal level of the AI - the AI guesses randomly
    until it hits something. Then it goes into target mode. Returns to hunt mode
    when the list of unresolved hits is empty. *)
let normal_attack state ai =
  if ai.on_hunt
  then (let tuple = State.random_ai_attack state in
        let new_state = fst tuple in
        let pos = snd tuple in
        if State.get_enemy_hits new_state > State.get_enemy_hits state
        then (new_state, update_ai ai pos false (pos::ai.unresolved_hits)
                                                      (pos_to_string pos) "" "")
        else (new_state, update_ai ai pos true ai.unresolved_hits "" "" ""))
  else let targ_tuple = target ai in
    let next_target = fst targ_tuple in
    let orient = snd targ_tuple in
    let new_state = State.ai_attack state next_target in
    let temp = State.get_player_status state new_state in
    let new_unresolved =
      if is_hit next_target state
      then remove_sunk_ships new_state (next_target::ai.unresolved_hits) temp
      else remove_sunk_ships new_state ai.unresolved_hits temp in
    if temp <> "" && new_unresolved = []
    then (new_state, update_ai ai next_target true [] "" "" "")
    else if is_hit next_target state
    then (if List.mem (State.helper ai.original_hit) new_unresolved
          then (new_state, update_ai ai next_target false (new_unresolved)
                            ai.original_hit (pos_to_string next_target) orient)
          else (let new_orig = (pos_to_string (choose new_unresolved
                                (Random.int (List.length new_unresolved)) 0)) in
                (new_state, update_ai ai next_target false
                                              (new_unresolved) new_orig "" "")))
    else (if ai.last_hit <> ""
          then (new_state, update_ai ai next_target false new_unresolved
                                         ai.original_hit "" ai.last_orientation)
          else (new_state, update_ai ai next_target false new_unresolved
                                                        ai.original_hit "" ""))

(** [hard_attack] takes in a [state] and an [ai]. Returns a tuple containing
    the new state and new ai. Hard level of the AI - the AI calculates the
    total possible orientations on each position of the board then attacks
    the position with the highest value. Goes into target mode when it hits
    something and returns to hunt mode when the list of unresolved hits is
    empty. *)
let hard_attack state ai =
    let board = generate_board state ai string_pos [] in
    if ai.on_hunt
        then (let pos = get_highest_value board in
              let new_state = State.ai_attack state pos in
        if State.get_enemy_hits new_state > State.get_enemy_hits state
        then (new_state, update_ai ai pos false (pos::ai.unresolved_hits)
                                                      (pos_to_string pos) "" "")
        else (new_state, update_ai ai pos true ai.unresolved_hits "" "" ""))
    else let targ_tuple = target ai in
      let next_target = fst targ_tuple in
      let orient = snd targ_tuple in
      let new_state = State.ai_attack state next_target in
      let temp = State.get_player_status state new_state in
      let new_unresolved =
        if is_hit next_target state
        then remove_sunk_ships new_state (next_target::ai.unresolved_hits) temp
        else remove_sunk_ships new_state ai.unresolved_hits temp in
      if temp <> "" && new_unresolved = []
      then (new_state, update_ai ai next_target true [] "" "" "")
      else if is_hit next_target state
      then (if List.mem (State.helper ai.original_hit) new_unresolved
            then (new_state, update_ai ai next_target false (new_unresolved)
                            ai.original_hit (pos_to_string next_target) orient)
            else (let new_orig = (pos_to_string (choose new_unresolved
                                (Random.int (List.length new_unresolved)) 0)) in
                  (new_state, update_ai ai next_target false
                                              (new_unresolved) new_orig "" "")))
      else (if ai.last_hit <> ""
            then (new_state, update_ai ai next_target false new_unresolved
                                        ai.original_hit "" ai.last_orientation)
            else (new_state, update_ai ai next_target false new_unresolved
                                                        ai.original_hit "" ""))

(** [update_ai] takes in an [ai] and a [pos] and it updates the [ai] by changing
    the board to reflect an attack at that position. *)
let update_master_ai ai pos =
  {
    last_hit = ai.last_hit;
    last_orientation = ai.last_orientation;
    board = update_board (ai.board) pos;
    level = ai.level;
    on_hunt = ai.on_hunt;
    unresolved_hits = ai.unresolved_hits;
    target_misses = ai.target_misses;
    next_hit = ai.next_hit;
    original_hit = ai.original_hit;
    direction = ai.direction;
  }

(** [master_attack] takes in a [state] and an [ai]. Returns a tuple containing
    the new state and new ai. Hardest level of AI as the AI knows where all of
    your ships are placed. *)
let master_attack state ai =
  let ship_pos = State.ship_positions state in
  let rec go_through lst =
    match lst with
    |[] -> failwith "done"
    |h::t -> if not(is_attacked ai.board h)
      then (ai_attack state h, update_master_ai ai h)
      else go_through t in
  go_through ship_pos

(** [attack] takes in an [ai] and a [state]. Calls either [easy_attack],
    [normal_attack], or [master_attack] based off the level of the [ai]. Returns
    a tuple containing the new state and new ai. *)
let attack ai state =
  if ai.level = "beginner" then easy_attack state ai
  else if ai.level = "amateur" then normal_attack state ai
  else if ai.level = "professional" then hard_attack state ai
  else master_attack state ai

(** [get_level] takes in an [ai] and returns the ai's level. *)
let get_level ai =
  ai.level