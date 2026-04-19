(* exception NoPlayers

(** is a placehodler *)
module Role = struct
  type t
end

(** Wrapper for indirecting {!kind}. *)
type mode =
  | Alignment
  | Role

(** {b Investigative} {!kind} are {i allied} with the {!townsfolk} and the ability to determine information about a target player. {i See {{:https://en.wikipedia.org/wiki/Mafia_(party_game)#Investigative_roles}here}.}
*)
type kind =
  | Alignment of Alignment.t
  (** Capable of detecting the alignment of another player. E.g., Detective, Seer, Commandant, Sherrif, Police, etc. {i See {{:https://en.wikipedia.org/wiki/Mafia_(party_game)#Investigative_roles_(standard)}here}.}
  *)
  | Role of Role.t
  (** Capable of detecting the role of another player. E.g., Psychic, Wizard, Fortune Teller, Oracle, Tracker, Watcher, etc. {i See {{:https://en.wikipedia.org/wiki/Mafia_(party_game)#Investigative_roles_(less_common)}here}.}
  *)

(** extracts information about player [x] depending on the [mode]. *)
let mode (x : (Role.t, Alignment.t) Player.t) : mode -> kind = function
  | Alignment -> Alignment x.alignment
  | Role -> Role x.role
;;

module type S = sig
  val name : string
  val mode : mode
  val alignment : Alignment.t

  (* NOTE: maybe this can be ommitted by [action] receiving the means of outputting itself. *)
  val is_public : bool
  val action : (Role.t, Alignment.t) Player.t list -> kind
end

module Make (X : sig
    val name : string
    val mode : mode
    val alignment : Alignment.t
    val is_public : bool
  end) : S = struct
  (** {b placeholder} selection algorithm. *)
  let selection (x : mode) : (Role.t, Alignment.t) Player.t list -> kind
    = function
    | [] -> raise NoPlayers
    | h :: _ -> mode h x
  ;;

  include X

  (** makes a {!selection} based on the {!mode}. *)
  let action (xs : (Role.t, Alignment.t) Player.t list) : kind =
    selection mode xs
  ;;
end

(** E.g., *)
module Examples = struct
  module Detective : S = Make (struct
      let name = "Detective"
      let mode : mode = Alignment
      let alignment = Alignment.Innocent
      let is_public = false
    end)
  module Oracle : S = Make (struct
      let name = "Oracle"
      let mode : mode = Role
      let alignment = Alignment.Innocent
      let is_public = false
    end)
end *)
