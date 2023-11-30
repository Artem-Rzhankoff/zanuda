(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Lexing

let shift_pos point content =
  let lines = String.split_on_char '\n' content in
  let len = List.length lines in
  let shift = String.length (List.nth lines (len - 1)) in
  match len with
  | 1 -> { point with pos_cnum = point.pos_cnum + shift }
  | _ -> { point with pos_lnum = point.pos_lnum + len - 1; pos_bol = 0; pos_cnum = shift }
;;
open Base

let is_whitespace = Char.is_whitespace

let lp pos = many_till any_char (string "(*") >>| 
  fun s -> let pos = shift_pos pos (String.of_char_list s) in pos, "(*"

let rp = string "*)"

let comm pos = 
  lift2
  (fun (pos, lp) s -> let comm = lp ^ s ^ "*)" in {pos with pos_cnum = pos.pos_cnum + 1}, shift_pos pos comm, comm)
  (lp pos)
  (many_till any_char rp >>| String.of_char_list )

let parse_comm pos = many (comm pos)

let parse pos content = 
  match parse_string ~consume:Consume.Prefix (parse_comm pos) content with 
  | Ok res -> res
  | _ -> []






