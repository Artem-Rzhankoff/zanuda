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
      set_padding (exp_start e) (pat_point pat Start)  Space_padding;
      set_padding (exp_start e) (exp_start e) (Padding "function"))
    ()
;;

let get_propose_function_payload ematch_case =
  let extra_arg =
    let c = first_case ematch_case in
    c.c_lhs
  in
  set_empty_padding (pat_point extra_arg Start) (pat_point extra_arg End)
;;

let apply_fix = function
  | Texp_function { cases } ->
    get_match_constr_payload cases;
    get_propose_function_payload cases
  | _ -> failwith "invalid_arg"
;;

let a x = 
  match x with(* *)
 (* afvasd *) | true -> 1
  | false -> 2

let a x = 
  let open Typedtree in 
  match x with
  | true -> 1
  | false -> 2


  let a x = 
    match x with    (*    *)
   (* afvasd *) | true -> 1
    | false -> 2

let a x = 
  match x with (*
 asfaefaw    *) true -> 1
     | false -> 2