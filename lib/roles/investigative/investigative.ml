
(** {b Investigative} {!kind} are {i allied} with the {!townsfolk} and the ability to determine information about a target player. {i See {{:https://en.wikipedia.org/wiki/Mafia_(party_game)#Investigative_roles}here}.} *)
type kind = 
| Alignment of Alignment.t
(** Capable of detecting the alignment of another player. E.g., Detective, Seer, Commandant, Sherrif, Police, etc. {i See {{:https://en.wikipedia.org/wiki/Mafia_(party_game)#Investigative_roles_(standard)}here}.} *)

| Role of Role.t
(** Capable of detecting the role of another player. E.g., Psychic, Wizard, Fortune Teller, Oracle, Tracker, Watcher, etc. {i See {{:https://en.wikipedia.org/wiki/Mafia_(party_game)#Investigative_roles_(less_common)}here}.} *)