type value =
  | Object of value list
  | List of string * value list
  | String of string
  | Int of int
  | Float of float
  | Bool of bool

type prog =
  | Prog of value list