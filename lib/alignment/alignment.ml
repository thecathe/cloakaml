type t =
  | Innocent
  | Outsider
  | Traveller
  | Evil

let allied (a : t) (b : t) : bool =
  match a, b with
  | Innocent, Innocent -> true
  | Outsider, Outsider -> true
  | Traveller, Traveller -> true
  | Evil, Evil -> true
  | _, _ -> false
;;
