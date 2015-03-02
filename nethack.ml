exception ParseFailure of string



type dungeon =
  | DungeonsOfDoom
  | Gehennom
  | GnomishMines
  | Quest
  | Sokoban
  | FortLudios
  | VladsTower
  | ElementalPlanes

type role =
  | Archeologist
  | Barbarian
  | Caveman
  | Healer
  | Knight
  | Monk
  | Priest
  | Ranger
  | Rogue
  | Samurai
  | Tourist
  | Valkyrie
  | Wizard

type race =
  | Dwarf
  | Elf
  | Gnome
  | Human
  | Orc

type gender =
  | Male
  | Female

type alignement =
  | Lawful
  | Neutral
  | Chaotic

type ending_informations =
    {
      score: int ;
      dungeon: dungeon ;
      dungeon_level : int ;
      maximum_level : int ;
      hit_points : int ;
      maximum_hit_points : int ;
      number_death : int ;
      role : role ;
      race : race ;
      gender : gender ;
      alignement : alignement ;
      pseudo : string ;
      cause : string ;
    }


let parse_dungeon = function
  | 0 -> DungeonsOfDoom
  | 1 -> Gehennom
  | 2 -> GnomishMines
  | 3 -> Quest
  | 4 -> Sokoban
  | 5 -> FortLudios
  | 6 -> VladsTower
  | 7 -> ElementalPlanes
  | _ -> raise (ParseFailure "Dungeon number not attributed")


let parse_role = function
  | "Arc" -> Archeologist
  | "Bar" -> Barbarian
  | "Cav" -> Caveman
  | "Hea" -> Healer
  | "Kni" -> Knight
  | "Mon" -> Monk
  | "Pri" -> Priest
  | "Ran" -> Ranger
  | "Rog" -> Rogue
  | "Sam" -> Samurai
  | "Tou" -> Tourist
  | "Val" -> Valkyrie
  | "Wiz" -> Wizard
  | _ -> raise (ParseFailure "Role not attributed")


let parse_race = function
  | "Dwa" -> Dwarf
  | "Elf" -> Elf
  | "Gno" -> Gnome
  | "Hum" -> Human
  | "Orc" -> Orc
  | _ -> raise (ParseFailure "Race not attributed")


let parse_gender = function
  | "Mal" -> Male
  | "Fem" -> Female
  | _ -> raise (ParseFailure "Gender not attributed")

let parse_alignement = function
  | "Law" -> Lawful
  | "Neu" -> Neutral
  | "Cha" -> Chaotic
  | _ -> raise (ParseFailure "Alignement not attributed")


let print_dungeon = function
  | DungeonsOfDoom  -> "in the Dungeons of Doom"
  | Gehennom        -> "in the Gehennom"
  | GnomishMines    -> "in the Gnomish Mines"
  | Quest           -> "during the Quest"
  | Sokoban         -> "in the Sokoban"
  | FortLudios      -> "in Fort Ludios"
  | VladsTower      -> "in Vlad's Tower"
  | ElementalPlanes -> "on the Elemental Planes"


let print_role = function
  | Archeologist -> "Archeologist"
  | Barbarian    -> "Barbarian"
  | Caveman      -> "Caveman"
  | Healer       -> "Healer"
  | Knight       -> "Knight"
  | Monk         -> "Monk"
  | Priest       -> "Priest"
  | Ranger       -> "Ranger"
  | Rogue        -> "Rogue"
  | Samurai      -> "Samurai"
  | Tourist      -> "Tourist"
  | Valkyrie     -> "Valkyrie"
  | Wizard       -> "Wizard"

let print_race = function
  | Dwarf -> "Dwarvish"
  | Elf   -> "Elven"
  | Gnome -> "Gnomish"
  | Human -> "Human"
  | Orc   -> "Orcish"

let print_gender = function
  | Male   -> "Male"
  | Female -> "Female"

let print_alignement = function
  | Lawful  -> "Lawful"
  | Neutral -> "Neutral"
  | Chaotic -> "Chaotic"



let parse_log_line line =
  Scanf.sscanf line ("%d.%d.%d %d %d %d %d %d %d %d %d %d %d %s %s %s %s %s@,%s@\n")
	      (fun _ _ _ score dnb dungeon_level maximum_level
		  hit_points maximum_hit_points number_death _ _ _
		  role race gender align pseudo cause ->

	      {
		score ;
		dungeon = parse_dungeon dnb ;
		dungeon_level ;
		maximum_level ;
		hit_points ;
		maximum_hit_points ;
		number_death ;
		role = parse_role role ;
		race = parse_race race;
		gender = parse_gender gender ;
		alignement = parse_alignement align ;
		pseudo ;
		cause
	      })
