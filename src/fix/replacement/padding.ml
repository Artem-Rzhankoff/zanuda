(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Location
open Lexing

let get_line_col loc =
  ( loc.loc_start.pos_lnum
  , loc.loc_start.pos_cnum - loc.loc_start.pos_bol
  , loc.loc_end.pos_lnum
  , loc.loc_end.pos_cnum - loc.loc_end.pos_bol )
;;

let check_loc loc lines =
  let sline, scol, eline, ecol = get_line_col loc in
  sline > 0
  && sline < Array.length lines
  && eline > 0
  && eline < Array.length lines
  && scol > 0
  && scol <= (String.length lines.(sline - 1) + 1)
  && ecol > 0
  && ecol <= (String.length lines.(eline - 1) + 1)
  && eline - sline >= 0
  && (eline != sline || ecol - scol >= 0)
;;

let space_padding loc flines =
  let sline, scol, eline, ecol = get_line_col loc in
  match sline = eline with
  | true ->
    let padding = String.make (ecol - scol) ' ' in
    padding
  | false ->
    let padding =
      String.make (String.length flines.(sline - 1) - scol) ' '
      |> fun s -> Format.sprintf "%s\n" s
    in
    let padding =
      Array.fold_left
        (fun pad s -> Format.sprintf "%s%s\n" pad (String.make (String.length s) ' '))
        padding
        (Array.sub flines sline (eline - sline - 1))
    in
    let padding = Format.sprintf "%s%s" padding (String.make ecol ' ') in
    padding
;;

let payload_between_repls (loc_end_buf, loc_start_repl) flines buf =
  let end_buf_line, end_buf_col =
    loc_end_buf.pos_lnum, loc_end_buf.pos_cnum - loc_end_buf.pos_bol
  in
  let repl_line, repl_col =
    loc_start_repl.pos_lnum, loc_start_repl.pos_cnum - loc_start_repl.pos_bol
  in
  let payload =
    match end_buf_line = repl_line with
    | true -> String.sub flines.(repl_line - 1) end_buf_col (repl_col - end_buf_col)
    | false ->
      let lines =
        String.sub
          flines.(end_buf_line - 1)
          end_buf_col
          (String.length flines.(end_buf_line - 1) - end_buf_col)
      in
      let lines =
        Array.fold_left
          (fun ls l -> Format.sprintf "%s%s\n" ls l)
          (Format.sprintf "%s%s" lines "\n")
          (Array.sub flines end_buf_line (repl_line - end_buf_line - 1))
      in
      let lines = lines ^ String.sub flines.(repl_line - 1) 0 repl_col in
      lines
  in
  Buffer.add_string buf payload;
  buf
;;