(** @canonical Game.Rounds.Round *)

(** tracks turn counter and players *)
type t =
  { (* phase : Phase.t ref
       ; *)
    players : Players.t (* ; rolemap : bool Roles.Map.t *)
  }

(* let phase (x : t) : Phase.t = !(x.phase) *)
let players (x : t) : Players.t = x.players
(* let rolemap (x : t) : bool Roles.Map.t = x.rolemap *)

let get_random_player (x : Roles.Role.Alignment.t) : Players.t -> Player.t =
  Players.random ~f:(Players.aligned x)
;;

let make
      (* ?(phase : Phase.t = Phase.Day) *)
      (* (rolemap : bool Roles.Map.t) *)
        (players : Players.t)
  : t
  =
  { players }
;;

(* let is_phase (x : t) : Phase.t -> bool = Phase.equal (phase x) *)

(*
   (** [is_tonight n x] is [true] if [x.phase.current] is {{!Phase.Night}Night} and [x.num] is equal to [n], i.e., [n] is the turn number collected in the {{!Phase.Day}Day} for an ability to be used at {{!Phase.Night}Night}.
   *)
   let is_tonight (n : int) (x : t) : bool = Int.equal n x.num && is_phase x Night

   (** [is_today n x] is {i dual} respective to {!is_tonight} but for {{!Phase.Day}Day}.
   *)
   let is_today (n : int) (x : t) : bool = Int.equal n x.num && is_phase x Day

   (** [is_tomorrow n x] returns [is_today (n + 1) x]. *)
   let is_tomorrow (n : int) (x : t) : bool = is_today (n + 1) x *)
