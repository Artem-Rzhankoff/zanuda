(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Base = Location

type location_t = Base.t =
  { loc_start : Lexing_visitor.position
  ; loc_end : Lexing_visitor.position
  ; loc_ghost : bool
  }

and 'a loc = 'a Base.loc =
  { txt : 'a
  ; loc : location_t
  }
[@@deriving
  visitors
    { variety = "iter"
    ; polymorphic = true
    ; monomorphic = [ "'env" ]
    ; ancestors = [ "Lexing_visitor.iter" ]
    ; nude = true
    }]
