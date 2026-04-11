exception Todo

type m = Empty | Has of (t * m)

and t = {
  state: state
}

and state = {
  players : player list 
}

and player = {
  id : player_id;
  name : string ;
  prof : player_class

}

and player_id = int

and player_class = {
  id : int;
  name : string;
  alignment : player_alignment
;  information : info list
}

and player_alignment = Good | Evil | Neutral | Wild

and player_status = Alive | Dead | Afflicted of player_affliction

and player_affliction = Cured | Sleep |  Possessed | Poisoned 

(* TODO: red-black tree for information shared *)
and info_tree = {
  win : info 
  ; lose : info
}

and info = | Nothing | Misinformation of info_kind | Disinformation of info_kind  | Information of info_kind 

and info_kind = Quantified of info_quantifiers

and info_quantifiers =
| HasAlignment of (player_quantifiers * player_alignment)
| HasAffliction of (player_quantifiers * player_affliction)

and player_quantifiers = 
  | AtLeastOneOf of (player_id list)
  | OneOf of (player_id list)
  | NoneOf of (player_id list)
  | AllOf of (player_id list)


(* module type S = sig
  
  (* type t = {
    prev : kind
  } *)

end
type kind = | Initial | Turn of (module S)

module Make (X:S) : S = struct
  
end

let initial () : t = Initial

let next : t -> t = function 
  | Initial -> raise Todo
  | Turn x -> raise Todo
;; *)