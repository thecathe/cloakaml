type trigger =
  | Each of Round.Phase.t
  (** Occurs each {!Round.Phase} (i.e., {{!Round.Phase.Day}Day} or {{!Round.Phase.Night}Night}).
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
  (** Automatically triggered on death. (E.g., the {{!Roles.Slayer}Slayer}.) *)
  | OnNomination
  (** Automatically triggered on nomination. (E.g., the {{!Roles.Virgin}Virgin}.)
  *)
  | OnDeath of time
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

and time =
  | Any
  | Night
  | Day
[@@deriving show { with_path = false }]

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
  | SpecificPlayer of Roles.t
  (** Ability targets a specific player. (E.g., the {{!Roles.FortuneTeller}FortuneTeller}.)
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

type ability =
  { trigger : trigger
  ; target : target
  ; yield : yield
  ; comment : string
  }
[@@deriving show { with_path = false }]

type t =
  { mutable active : ability list
  ; mutable inactive : ability list
  ; mutable disabled : bool
  }
[@@deriving show { with_path = false }]

exception ToDo

(* https://ocaml.org/manual/5.4/effects.html *)
type _ Effect.t += NeedRolesToTarget : unit -> Roles.t Effect.t
type _ Effect.t += AddExtraOutsider : unit -> unit Effect.t

let get : Roles.t -> ability list =
  let open Effect in
  (* let open Effect.Deep in *)
  function
  | Empath ->
    [ { trigger = Each Night
      ; target = Neighbours
      ; yield = NumOf (Alignment Evil)
      ; comment = "Each Night, learn how many of your two neighbours are Evil."
      }
    ]
  | FortuneTeller ->
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
  | Baron ->
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
  | Investigator ->
    [ { trigger = StartOfGame
      ; target = Any 2
      ; yield = Learn (Either (Kind Minion))
      ; comment =
          "You start knowing that 1 of 2 players is a particular Minion."
      }
    ]
  | Undertaker ->
    [ { trigger = Each Night
      ; target = Situational Executed
      ; yield = Learn (Exact Role)
      ; comment =
          "Each Night, you learn which Role died by execution the previous Day."
      }
    ]
  | Ravenkeeper ->
    [ { trigger = OnDeath Night
      ; target = Player
      ; yield = Learn (Exact Role)
      ; comment =
          "If you die at Night, you are woken to choose a player: you learn \
           their Role."
      }
    ]
  | _ -> raise ToDo
;;

let make (x : Roles.t) : t = { active = get x; inactive = []; disabled = false }
