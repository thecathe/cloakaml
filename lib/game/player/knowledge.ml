type t =
    | Just of data
    | Either of data * data

  and data =
    | PlayerIsRole of int * Roles.Role.t
    | PlayerIsAligned of int * Roles.Alignment.t
    | PlayerIsKind of int * Roles.Kind.t