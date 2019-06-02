type move =
  | Quit
  | Attack of string
  | Place of string * string * string

exception Empty

exception Illegal


(** [delete_spaces lst acc] is a recursive helper function for [parse].
    Given a list and accumulator, it returns a list with all the empty strings
    removed from the original list. *)
let rec delete_spaces lst acc =
  match lst with
  | [] -> acc
  | h::t ->
      if h = ""
      then delete_spaces t acc
      else delete_spaces t (h::acc)

(* [parse str] is a function that parses a player's input into a move. This
    function takes the string given and makes the first word the verb, and the
    collection of the following words the move phrase.
    Raises [Empty] if the string is the empty string or
    contains only spaces, tabs, enters, etc.
    Raises [Illegal] if the move is illegal. Illegal is defined as:
   - the verb is neither "quit" or "attack" or "place"
   - the verb is "quit" and there are words after it
   - the verb is "attack" and there are no words after
   - the verb is "place" and there are no words after it
   - the verb is "place" and there are more than three following words
   - the verb is "attack" and there is more than one following word. *)
let parse str =
  let str_list = List.rev (delete_spaces (String.split_on_char ' ' str) []) in
  match str_list with
  | [] -> raise Empty
  | h::[] ->
      if h = "quit"
      then Quit
      else raise Illegal
  | attack::pos::[] ->
      if attack = "attack"
      then Attack (pos)
      else raise Illegal
  | place::ship::orient::pos::[] ->
      if place = "place"
      then Place ((String.lowercase_ascii ship), orient, pos)
      else raise Illegal
  | _ -> raise Illegal

