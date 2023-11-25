(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Base = Ident

type ident_t = (Base.t[@opaque]) [@@deriving visitors { variety = "iter"; nude = true }]
