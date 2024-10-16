val test_countries : string list
(*[test_countries] is a small list of countries to test functionality*)

val num_players : int
(** [num_players] is an int ref that holds the number of players in a particular
    game *)

val countries : string list
(*[countries] is a list of the names of every country in Risk*)

val soldiers : int array
(*[soldiers] contains the number of starting troops for different numbers of
  players. For example, [soldiers.(i)] is the number of troops for [(i+2)]
  players*)

val claim_countries : string list -> int -> unit
(**[claim_countries rem_countries player_turn] lets player [player_turn] claim a
   country from the list of remaining countries [rem_countries]. The countries
   claimed is stored as a association list where the keys are the countries and
   the values are the player that claimed them.*)

val worldMap : (string * Map.country) list
(**[worldMap] is an a list of country names and country type value. The names
   are redundant, but are used for simple pattern matching *)

val troops_state : int array
(** [troops_state] is an int array initialised to the starting number of troops
    for each player, depending on the number of players *)

val print_owners : unit -> unit
(**[[print_owners ()] prints each country and which player owns it]*)

val print_rules : unit -> unit
(** [print_rules] prints out a summary of the game rules and stages of Risk. *)

val number_of_players : bool -> int
(** [number_of_players first] prompts for and returns the number of players. If
    [first] is true, it also prints the game rules. *)

val bonusList : (string * int ref) list
(** [bonusList] is a list of continents and their respective bonus troop values. *)

val remove_country : string -> string -> bool
(** [remove_country country1 country2] returns true if [country1] is not equal
    to [country2]. *)

val auto_assign : string list -> int -> unit
(** [auto_assign rem_countries player_turn] automatically assigns countries to
    players in a round-robin fashion until all are claimed. *)

val subTroops : int -> int -> unit
(** [subTroops player_index n] subtracts [n] troops from the troop count of the
    player at [player_index]. *)

val addTroops : int -> int -> unit
(** [addTroops player_index n] adds [n] troops to the troop count of the player
    at [player_index]. *)
