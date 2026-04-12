    (** tracks turn counter and players *)
    type t =
      { num : int
      ; players : Players.t
      }

    let initial (players : Players.t) : t = { num = 0; players }
