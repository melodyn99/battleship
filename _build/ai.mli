open State

type pos_info = {
  priority:int;
  attack:bool;
}

type position = State.position

type t = {
  last_hit: string;
  last_orientation: string;
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

val init_ai: string -> t

val attack: t -> State.t -> (State.t * t)

val get_level: t -> string