(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Base = Longident

type longident_t = Base.t =
  | Lident of (string[@opaque])
  | Ldot of longident_t * (string[@opaque])
  | Lapply of longident_t * longident_t
[@@deriving visitors { variety = "iter"; nude = true }]
