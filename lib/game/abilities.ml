(** {1 Abilities} *)

module Ability = struct
  module Trigger = struct
    module Time = struct
      type t =
        | Any
        | Night
        | Day
      [@@deriving show { with_path = false }]

      exception AnyPhase

      let phase : t -> Phase.t = function
        | Any -> raise AnyPhase
        | Night -> Phase.Night
        | Day -> Phase.Day
      ;;
    end

    type t =
      | Each of Phase.t
      (** Occurs each {!Phase} (i.e., {{!Phase.Day}Day} or {{!Phase.Night}Night}).
      *)
      | GameSetup
      (** Occurs {i before} the game starts, during {i {{!Game.Setup}Setup}}. (E.g., the {{!Roles.Baron}Baron} or the {{!Roles.Drunk}Drunk}.)
      *)
      | Passive
      (** An ambient, passive effect. Always active unless overridden. (E.g., the {{!Roles.Recluse}Recluse} or the {{!Roles.Soldier}Soldier}.)
      *)
      | StartOfGame
      (** Automatically triggered when the game starts. (E.g., the {{!Roles.Investigator}Investigator}.)
      *)
      | OncePerGame
      (** Manually triggered by the player, once per game. (E.g., the {{!Roles.Slayer}Slayer}.)
      *)
      | OnExecution
      (** Automatically triggered on death. (E.g., the {{!Roles.Slayer}Slayer}.)
      *)
      | OnNomination
      (** Automatically triggered on nomination. (E.g., the {{!Roles.Virgin}Virgin}.)
      *)
      | OnDeath of Time.t
      (** Automatically triggered on death. (E.g., the {{!Roles.Ravenkeeper}Ravenkeeper}.)
      *)
      | WhenTargetted
      (** Automatically triggered when {i targetted} by another players ability. (E.g., the {{!Roles.Spy}Spy}.)
      *)
      | WhenTargetting
      (** Automatically triggered when {i targetting} another player with your ability. (E.g., the {{!Roles.FortuneTeller}FortuneTeller}.)
      *)
      | PlayerStates
      (** Automatically triggered when a condition on the players is met. (E.g., the {{!Roles.ScarletWoman}ScarletWoman}.)
      *)
    [@@deriving show { with_path = false }]

    let active ?(phase : Phase.t option = None) : t -> bool = function
      | Passive -> true
      | Each x -> (match phase with None -> false | Some y -> Phase.equal x y)
      | _ -> false
    ;;
  end

  type target =
    | Nobody
    (** Ability does not involve selecting a target. (E.g., the {{!Roles.Empath}Empath}.)
    *)
    | Self
    | Any of int
    (** Ability targets a number of {i any} players. (E.g., the {{!Roles.FortuneTeller}FortuneTeller}.)
    *)
    | Players of int
    (** Ability targets a number of players. (E.g., the {{!Roles.FortuneTeller}FortuneTeller}.)
    *)
    | Neighbours
    (** Ability targets a players neighbours. (E.g., the {{!Roles.Empath}Empath}.)
    *)
    | Player
    | SpecificPlayer of int
    (** Ability targets a specific player with id. (E.g., the {{!Roles.FortuneTeller}FortuneTeller}.)
    *)
    | OtherPlayer of Roles.t
    (** Ability targets {i another} player. (E.g., the {{!Roles.Monk}Monk}.) *)
    | Situational of situation
  [@@deriving show { with_path = false }]

  and situation =
    | Executed
    | Nominator
  [@@deriving show { with_path = false }]

  type yield =
    | Fun of (unit -> yield)
    | Nothing
    | NumOf of trait
    | Learn of learn
    | Just of trait
    | Safe (** Unlike {!Learn} this information does not have to be accurate. *)
  [@@deriving show { with_path = false }]

  and learn =
    | Either of trait
    | Exact of trait
  [@@deriving show { with_path = false }]

  and trait =
    | Role
    | Kind of Roles.kind
    | Alignment of Roles.alignment
  [@@deriving show { with_path = false }]

  type t =
    { trigger : Trigger.t
    ; target : target
    ; yield : yield
    ; comment : string
    }
  [@@deriving show { with_path = false }]
end

type t =
  { mutable active : Ability.t list
  ; mutable inactive : Ability.t list
  ; mutable disabled : bool
  }
[@@deriving show { with_path = false }]

(** {1 Role Abilities} *)

(** [active ?phase x] returns the subset of {!t.active} that have a {{!Ability.Trigger.t}Trigger} that are active. If [phase] is [Some y] then the returned list incldues those abilities triggered {{!Ability.Trigger.Each}Each} [phase].
*)
let active ?(phase : Phase.t option) (x : t) : Ability.t list =
  List.filter
    (fun (z : Ability.t) -> Ability.Trigger.active ~phase z.trigger)
    x.active
;;

(** [phase phase x] returns the subset of {!t.active} that are triggered {{!Ability.Trigger.Each}Each} [phase]. {i See {!val:active}.}
*)
let phase (phase : Phase.t) : t -> Ability.t list = active ~phase

module Abilities = struct
  exception ToDo

  (* let raise_todo = raise ToDo *)
  let raise_todo = []

  open Effect
  open Effect.Deep

  (* https://ocaml.org/manual/5.4/effects.html *)
  type _ Effect.t +=
    | AddExtraOutsider : unit -> unit Effect.t
    | NeedRolesToTarget : unit -> int Effect.t

  (** {2 Townsfolk} *)

  let washerwoman () : Ability.t list =
    [ { trigger = StartOfGame
      ; target = Any 2
      ; yield = Learn (Either (Kind Townsfolk))
      ; comment =
          "You start knowing that 1 of 2 players is a particular Townsfolk."
      }
    ]
  ;;

  let librarian () : Ability.t list = raise_todo

  let investigator () : Ability.t list =
    [ { trigger = StartOfGame
      ; target = Any 2
      ; yield = Learn (Either (Kind Minion))
      ; comment =
          "You start knowing that 1 of 2 players is a particular Minion."
      }
    ]
  ;;

  let chef () : Ability.t list = raise_todo

  let empath () : Ability.t list =
    [ { trigger = Each Night
      ; target = Neighbours
      ; yield = NumOf (Alignment Evil)
      ; comment = "Each Night, learn how many of your two neighbours are Evil."
      }
    ]
  ;;

  let fortune_teller () : Ability.t list =
    [ { trigger = Each Night
      ; target = Players 2
      ; yield = Learn (Either (Kind Demon))
      ; comment =
          "Each Night, choose 2 players: you learn if either is a Demon."
      }
    ; { trigger = WhenTargetting
      ; target = SpecificPlayer (perform (NeedRolesToTarget ()))
      ; yield = Just (Kind Demon)
      ; comment =
          "The Red Herring: There is a Good player that registers as a Demon \
           to you."
      }
    ]
  ;;

  let undertaker () : Ability.t list =
    [ { trigger = Each Night
      ; target = Situational Executed
      ; yield = Learn (Exact Role)
      ; comment =
          "Each Night, you learn which Role died by execution the previous Day."
      }
    ]
  ;;

  let monk () : Ability.t list = raise_todo

  let ravenkeeper () : Ability.t list =
    [ { trigger = OnDeath Night
      ; target = Player
      ; yield = Learn (Exact Role)
      ; comment =
          "If you die at Night, you are woken to choose a player: you learn \
           their Role."
      }
    ]
  ;;

  let virgin () : Ability.t list = raise_todo
  let slayer () : Ability.t list = raise_todo
  let soldier () : Ability.t list = raise_todo
  let mayor () : Ability.t list = raise_todo

  (** {2 Outsiders} *)

  let butler () : Ability.t list = raise_todo
  let drunk () : Ability.t list = raise_todo
  let recluse () : Ability.t list = raise_todo
  let saint () : Ability.t list = raise_todo

  (** {2 Minions} *)

  let poisoner () : Ability.t list = raise_todo
  let spy () : Ability.t list = raise_todo
  let scarlet_woman () : Ability.t list = raise_todo

  let baron () : Ability.t list =
    [ { trigger = GameSetup
      ; target = Nobody
      ; yield =
          Fun
            (fun () ->
              perform (AddExtraOutsider ());
              perform (AddExtraOutsider ());
              Nothing)
      ; comment = "There are extra Outsiders in play (+2)."
      }
    ]
  ;;

  (** {2 Demons} *)

  let imp () : Ability.t list = raise_todo

  (** {2 Get Role Ability} *)

  let get : Roles.t -> Ability.t list = function
    (* townsfolk *)
    | Washerwoman -> washerwoman ()
    | Librarian -> librarian ()
    | Investigator -> investigator ()
    | Chef -> chef ()
    | Empath -> empath ()
    | FortuneTeller -> fortune_teller ()
    | Undertaker -> undertaker ()
    | Monk -> monk ()
    | Ravenkeeper -> ravenkeeper ()
    | Virgin -> virgin ()
    | Slayer -> slayer ()
    | Soldier -> soldier ()
    | Mayor -> mayor ()
    (* outsiders *)
    | Butler -> butler ()
    | Drunk -> drunk ()
    | Recluse -> recluse ()
    | Saint -> saint ()
    (* minions *)
    | Poisoner -> poisoner ()
    | Spy -> spy ()
    | ScarletWoman -> scarlet_woman ()
    | Baron -> baron ()
    (* demons *)
    | Imp -> imp ()
  ;;
end

let make (x : Roles.t) : t =
  { active = Abilities.get x; inactive = []; disabled = false }
;;
