(*AF:[country] represents one "country" from the game risk. Every [country] has
  a [name]. [ player] is the player who controls [country], [continent] is the
  continent [country] is in Risk. [troops] represents the number of troops in
  that country. [neighbours] represents the countries directly adjacent to
  [country] on the Risk game board*)
(*RI: [name] must be a valid country name from Risk. [player] must be [None] or
  [Some x] where 2<=[x]<=6. [continent] must be a valid continent name from Risk
  which contains [country]. [troops] must be [None] or [Some x] where [x] >= 1.
  [neighbours] must only contain countries adajacent to the country on the Risk
  game board*)
type country = {
  name : string;
  mutable player : int option;
  continent : string;
  mutable troops : int option;
  mutable neighbours : country list;
}

val read_lines : string -> (string * string) list
(*[read_lines file] is a tuple list where each tuple [(country,continent)] is a
  [country] in risk and the [continent] it belongs to. This information is read
  from [file] and requires that each line of [file] is of the form
  [country,continent]*)

val isNeighbour : country -> country -> bool
(*[isNeighbour c1 c2] returns [true] when [c1] and [c2] are adjacent/neighbours
  and [false] otherwise*)

val construct_map_element : string * string -> string * country
(*[construct_map_element a,b] is a country with name [a] on continent [b]*)

val map : unit -> (string * country) list
(**[map]() is an a list of country names and country type value. The names are
   redundant, but are used for simple pattern matching *)

val make : unit -> unit
(*[make ()] makes the map*)
