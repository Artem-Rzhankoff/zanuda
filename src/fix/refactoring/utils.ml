(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Location
open Typedtree
open Replacement.Refill

(*[TEMPORARY] we plan use a combination if dune diff and promote (maybe git variant) for user preview*)
module Report = struct
  let console loc ms =
    let open Zanuda_core.Utils in
    let fname = loc.loc_start.pos_fname in
    let msg ppf s = Format.fprintf ppf "%s\n%!" s in
    Report.txt ~loc ~filename:fname Format.std_formatter msg ms
  ;;
end

type end_point =
  | Start
  | End

type point =
  { loc : Location.t
  ; pos : end_point
  }

let position { loc; pos } =
  match pos with
  | Start -> loc.loc_start
  | End -> loc.loc_end
;;

let pat_point p point = 
  {loc = p.pat_loc; pos = point}

let exp_point e point =
  { loc = e.exp_loc; pos = point }
;;

let exp_start e = 
  exp_point e Start
;;

let exp_end e = 
  exp_point e End
;;

let gen_loc spoint epoint =
  { loc_start = position spoint; loc_end = position epoint; loc_ghost = false } (* check loc_ghost *)
;;

let fname loc = loc.loc_start.pos_fname

(* msg потом уберутся, когда что-то поопрятнее придумается *)
let set_payload ({ location; _ } as r) msg =
  Report.console location msg;
  add (fname location) r
;;

let set_padding p1 p2 payload msg =
  let location = gen_loc p1 p2 in
  set_payload { location; payload } msg
;;

let set_empty_padding p1 p2 msg = set_padding p1 p2 Void msg

let gen_constr_loc point offset =
  let open Lexing in
  let loc_start = point.loc_start in
  let loc_end =
    { pos_fname = loc_start.pos_fname
    ; pos_lnum = loc_start.pos_lnum
    ; pos_bol = loc_start.pos_bol
    ; pos_cnum = loc_start.pos_cnum + offset
    }
  in
  { point with loc_end }
;;
