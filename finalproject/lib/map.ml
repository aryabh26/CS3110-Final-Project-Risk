type country = {
  name : string;
  mutable player : int option;
  continent : string;
  mutable troops : int option;
  mutable neighbours : country list;
}

let construct_map_element (nm, cont) =
  ( nm,
    {
      name = nm;
      player = None;
      continent = cont;
      troops = None;
      neighbours = [];
    } )

let read_lines filename =
  let channel = open_in filename in
  let rec read_lines_helper acc =
    try
      let line = input_line channel in
      let parts = String.split_on_char ',' (String.trim line) in
      match parts with
      | [ territory; continent ] ->
          read_lines_helper
            ((String.trim territory, String.trim continent) :: acc)
      | _ -> read_lines_helper acc
    with End_of_file ->
      close_in channel;
      List.rev acc
  in
  read_lines_helper []

let territory_country_alist () = read_lines "lib/countries.txt"
let map () = List.map construct_map_element (territory_country_alist ())

let add_link (cont1, cont2) =
  try
    let country1 = List.assoc cont1 (map ()) in
    let country2 = List.assoc cont2 (map ()) in
    country1.neighbours <- country2 :: country1.neighbours;
    country2.neighbours <- country1 :: country2.neighbours
  with _ -> print_endline (cont1 ^ cont2)

let link_list () = read_lines "lib/links.txt"
let make () = List.iter add_link (link_list ())
let isNeighbour c1 c2 = List.mem c1 c2.neighbours
(* let updateOwner c p = c.player <- p (let convertoption = function |Some x ->
   x |_ -> failwith "No troops"); in let updateTroops c change = c.troops <-
   c.troops + convertoption change *)
