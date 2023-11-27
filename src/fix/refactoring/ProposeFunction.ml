(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Tast_pattern
open Utils
open Typedtree

let first_case cs = List.nth cs 0

type fix_kind =
  | Extra_argument
  | Verbose_match

let msg = function
  | Extra_argument ->
    Format.sprintf
      "(Fix `Propose_function` lint)\n%s"
      "Removing extra argument of an expression with replacing `match .. with ` to \
       `function`"
  | Verbose_match ->
    Format.sprintf
      "(Fix `Propose_function` lint)\n%s"
      "This pattern matching will be replaced by an equivalent and more concise \
       construction `function`"
;;

let get_match_constr_payload ematch_case =
  let e =
    let c = first_case ematch_case in
    c.c_rhs
  in
  let pat = texp_match (texp_ident __) __ in
  parse
    pat
    e.exp_loc
    e
    (fun _ cs () ->
      let pat =
        let c = first_case cs in
        c.c_lhs
      in
      let cloc = gen_loc e.exp_loc pat.pat_loc (Start, Start) in
      let funcloc = gen_constr_loc e.exp_loc 8 in
      (* тут еще надо будет чекнуть комменты *)
      set_payload { location = funcloc; payload = Padding "function" } (msg Verbose_match);
      set_payload
        { location =
            { cloc with
              loc_start = { cloc.loc_start with pos_cnum = cloc.loc_start.pos_cnum + 8 }
            }
        ; payload = Space_padding
        }
        (msg Verbose_match))
    ()
;;

let get_propose_function_payload ematch_case =
  let extra_arg =
    let c = first_case ematch_case in
    c.c_lhs
  in
  let cloc = gen_loc extra_arg.pat_loc extra_arg.pat_loc (Start, End) in
  set_payload { location = cloc; payload = Void } (msg Extra_argument)
;;

let get_loc = function
  | Texp_function { cases } ->
    get_match_constr_payload cases;
    get_propose_function_payload cases
  | _ -> failwith "invalid_arg"
;;
