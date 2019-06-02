type move =
  | Quit
  | Attack of string
  | Place of string*string*string

exception Empty

exception Illegal

val parse : string -> move