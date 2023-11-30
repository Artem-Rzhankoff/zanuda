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
  && scol <= String.length lines.(sline - 1) + 1
  && ecol > 0
  && ecol <= String.length lines.(eline - 1) + 1
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

let payload_between_repls (loc_end_buf, loc_start_repl) flines =
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
          (Format.sprintf "%s\n" lines)
          (Array.sub flines end_buf_line (repl_line - end_buf_line - 1))
      in
      let lines = lines ^ String.sub flines.(repl_line - 1) 0 repl_col in
      lines
  in
  payload
;;

let payload_between_repls_buf locs flines buf =
  let payload = payload_between_repls locs flines in
  Buffer.add_string buf payload;
  buf
;;

let insert_comments ({ loc_start; loc_end; _ } as loc) fcontent =
  let flines = Array.of_list (String.split_on_char '\n' fcontent) in
  let chunk = payload_between_repls (loc_start, loc_end) flines in
  let space_padding = space_padding loc flines in
  let space_lines = Array.of_list (String.split_on_char '\n' space_padding) in
  let comments = Comments_parser.parse loc_start chunk in
  let shift n pos_comm =
    if n = loc_start.pos_lnum
    then pos_comm - (loc_start.pos_cnum - loc_start.pos_bol)
    else pos_comm
  in
  match List.length comments with
  | 0 -> space_padding
  | _ ->
    List.iter
      (fun (spos, epos, comm) ->
        let sline, scol, eline, ecol =
          get_line_col { loc_start = spos; loc_end = epos; loc_ghost = false }
        in
        let rel_spos = shift sline scol in
        let rel_epos = shift eline ecol in
        let line = spos.pos_lnum - loc_start.pos_lnum in
        let string = space_lines.(line) in
        let new_string =
          Printf.sprintf "%s%s%s"
          (String.sub string 0 (rel_spos - 1))
          comm
          (String.sub string (rel_epos - 1) (String.length string - rel_epos))
        in
        Array.set space_lines line new_string)
      comments;
    let padding =
      Array.fold_left
        (fun content s -> Printf.sprintf "%s%s\n" content s)
        ""
        (Array.sub space_lines 0 (Array.length space_lines - 1))
    in
    padding ^ space_lines.(Array.length space_lines - 1)
;;
