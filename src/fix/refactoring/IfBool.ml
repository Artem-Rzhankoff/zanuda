(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree
open Utils

type ite =
  | If of bool
  | Then of bool
  | Else of bool

let bool_value e =
  let open Tast_pattern in
  parse ebool e.exp_loc ~on_error:(fun _ () -> None) e (fun b () -> Some b) ()
;;

type fix_kind =
  | Unwise_conjuction of bool
  | Unwise_ite of ite

type msg_kind =
  | Ite
  | Conj

let msg = function
  | Conj ->
    Format.sprintf
      "(Fix `If_bool` lint)\n%s"
      "This boolean expression will be replaced by an equivalent with removing unwise \
       conjunction"
  | Ite ->
    Format.sprintf
      "(Fix `If_bool` lint):\n%s"
      "This boolean expression will be replaced by an equivalent with removing \
       unwise`if_then_else`"
;;

let check_bool args vbool =
  let _, val1 = List.nth args 0 in
  let _, val2 = List.nth args 1 in
  let open Tast_pattern in
  match val1, val2 with
  | Some e1, Some e2 ->
    parse
      ebool
      e1.exp_loc
      ~on_error:(fun _ () ->
        match vbool with
        | true ->
          set_payload
            { location = gen_loc e1.exp_loc e2.exp_loc (End, End); payload = Default }
            (msg Conj)
        | false ->
          set_payload
            { location = gen_loc e1.exp_loc e2.exp_loc (Start, Start); payload = Default }
            (msg Conj))
      e1
      (fun _ () ->
        match vbool with
        | true ->
          set_payload
            { location = gen_loc e1.exp_loc e2.exp_loc (Start, Start); payload = Default }
            (msg Conj)
        | false ->
          set_payload
            { location = gen_loc e1.exp_loc e2.exp_loc (End, End); payload = Default }
            (msg Conj))
      ()
  | _ -> failwith "invalid_arg"
;;

let get_ite_loc e ie te ee pbool_site =
  let match_ite = function
    | If true, _ ->
      (* if true then x else y --> x *)
      set_payload
        { location = gen_loc e.exp_loc te.exp_loc (Start, Start); payload = Default }
        (msg Ite);
      (* поменять *)
      set_payload
        { location = gen_loc te.exp_loc e.exp_loc (End, End); payload = Default }
        (msg Ite)
    | If false, _ (* if false then x else y --> y *)
    | Then true, Some true (* if val then true else true --> true *)
    | Then false, Some false ->
      (* if val then false else false --> false*)
      set_payload
        { location = gen_loc e.exp_loc ee.exp_loc (Start, Start); payload = Default }
        (msg Ite)
    | Then true, Some false (* if val then true else false --> val *) ->
      set_payload
        { location = gen_loc e.exp_loc ie.exp_loc (Start, Start); payload = Default }
        (msg Ite);
      set_payload
        { location = gen_loc ie.exp_loc e.exp_loc (End, End); payload = Default }
        (msg Ite)
    | Then false, Some true ->
      (* if val then false else true --> not val *)
      ()
    | _ ->
      (* previous p-m covers cases when ebool was parsed in then-e*)
      ()
  in
  match_ite (pbool_site, bool_value ee)
;;

type a =
  | A of bool
  | B of bool
  | C of bool

let get_loc exp = function
  | Unwise_conjuction ebool ->
    (match exp.exp_desc with
     | Texp_apply (_, args) -> check_bool args ebool
     | _ -> failwith "invalid_arg")
  | Unwise_ite ite_type ->
    (match exp.exp_desc with
     | Texp_ifthenelse (ie, te, ee) -> get_ite_loc exp ie te (Option.get ee) ite_type
     | _ -> failwith "invalid_arg")
;;
