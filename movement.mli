open Game

val (+:) : position -> position -> position

val neighbours : position -> position list

val is_valid : game_state -> position -> bool
val valid_neighbours : game_state -> position -> position list

val shortest_path : game_state -> position -> position -> (case -> bool) -> position option * int

val find_power_up : grid -> position list
val find_productivity_up : grid -> position list
val find_all_bonus : grid -> position list
val find_empty : grid -> position list
val is_free : case -> bool
