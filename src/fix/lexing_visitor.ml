module Base = Lexing

type position = Base.position =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }
[@@deriving visitors { variety = "iter" }]
