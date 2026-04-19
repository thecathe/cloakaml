type t =
  | Role of Role.t
  | Kind of Kind.t
  | Alignment of Alignment.t
[@@deriving show { with_path = false }, eq]
