(** [position] represents a location on the grid.*)
type position = {
  row: char;
  col: int;
}

(** [board] represents the entire grid. It contains all the ship positions, the
    hits, misses, number of hits, and the list of ships that have not been
    sunken. *)
type board = {
  ships_pos: (string * (position list)) list;
  hits: (string * (position list)) list;
  misses: position list;
  num_hits: int;
  not_sunken: string list;
}

(** [t] represents the entire state of the game. It contains the name of the
    player, the player's board, the enemy's board, and the list of available
    ships. *)
type t = {
  name: string;
  player: board;
  enemy: board;
  avail: string list;
}

exception InvalidShipName

exception InvalidPlacement

exception InvalidOrientation

exception InvalidAttackPos

val length_of_ship: string -> int

val player_place: t -> string -> string -> string -> t

val init_state: string -> t

val player_attack: t -> string -> t

val ai_attack: t -> position -> t

val random_ai_attack: t -> t * position

val remaining: t -> int

val get_player_sign: t -> string -> string

val get_enemy_sign: t -> string -> string

val player_win: t -> bool

val ai_win: t -> bool

val get_status: t -> t -> string

val get_player_status: t -> t -> string

val get_name: t -> string

val print_enemy: t -> unit

val print_player: t -> unit

val print_ships: t -> unit

val get_not_sunken: t -> string list

val helper: string -> position

val in_bounds: position -> bool

val ship_positions: t -> position list

val get_ship_pos: t -> (string * (position list)) list

val get_enemy_hits: t -> int

val list_rem: 'a -> 'a list -> 'a list -> 'a list

val rand_pos: unit -> position