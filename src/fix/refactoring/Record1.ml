(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Location

let a loc l_ghost = {loc_start = loc.loc_start; loc_end = loc.loc_end; loc_ghost = l_ghost}