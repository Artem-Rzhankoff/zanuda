(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Base = Env

type env_t = (Base.t[@opaque]) [@@deriving visitors { variety = "iter"; nude = true }]
