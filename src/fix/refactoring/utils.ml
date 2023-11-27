(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Location
open Typedtree
open Replacement.Refill

type pos =
  | Start
  | End

(*[TEMPORARY] we plan use a combination if dune diff and promote (maybe git variant) for user preview*)
module Report = struct
  let console loc ms =
    let open Zanuda_core.Utils in
    let fname = loc.loc_start.pos_fname in
    let msg ppf s = Format.fprintf ppf "%s\n%!" s in
    Report.txt ~loc ~filename:fname Format.std_formatter msg ms
end 

let gen_loc spoint epoint = function
  | Start, Start ->
    { loc_start = spoint.loc_start; loc_end = epoint.loc_start; loc_ghost = false }
  | Start, End ->
    { loc_start = spoint.loc_start; loc_end = epoint.loc_end; loc_ghost = false }
  | End, Start ->
    { loc_start = spoint.loc_end; loc_end = epoint.loc_start; loc_ghost = false }
  | End, End ->
    { loc_start = spoint.loc_end; loc_end = epoint.loc_end; loc_ghost = false }
;;

let gen_constr_loc point offset = 
  let open Lexing in
  let loc_start = point.loc_start in
  let loc_end = {pos_fname = loc_start.pos_fname; pos_lnum = loc_start.pos_lnum; pos_bol = loc_start.pos_bol; pos_cnum = loc_start.pos_cnum + offset} in
  {point with loc_end = loc_end }

let set_payload ({location; _} as r) msg = 
  Report.console location msg;
  add location.loc_start.pos_fname r
;;

let set_payload_exp e1 e2 pos payload = 
  let open Typedtree in
  set_payload {location = gen_loc e1.exp_loc e2.exp_loc pos; payload }

