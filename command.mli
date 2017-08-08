open Game

val init_game : string -> (game_state -> unit) -> unit
val nop : unit -> unit
val move_to : position -> position -> unit
val deploy : unit -> unit
