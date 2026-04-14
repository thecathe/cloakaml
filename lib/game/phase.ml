  type t =
    | Day
    | Night
  [@@deriving show { with_path = false }, eq]

  let dual : t -> t = function Day -> Night | Night -> Day

  type data =
    { starting : t
    ; mutable current : t
    }
  [@@deriving show { with_path = false }, make]

  let make (starting : t) : data = make_data ~starting ~current:starting

  (* *)
  let is_start_phase (x : data) : bool = equal x.starting x.current
  let is_end_phase (x : data) : bool = Bool.not (is_start_phase x)

  exception StartNewRound

  let step (x : data) : data =
    if equal x.starting x.current
    then (
      x.current <- dual x.current;
      x)
    else raise StartNewRound
  ;;