(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Location
open Lexing

type payload =
  | Void
  | Space_padding
  | Padding of string

module OrderedType = struct
  type t =
    { location : Location.t
    ; payload : payload
    }

  let compare
    { location = { loc_start = { pos_cnum = p; _ }; _ }; _ }
    { location = { loc_start = { pos_cnum = p'; _ }; _ }; _ }
    =
    p - p'
  ;;
end

module Set = Set.Make (OrderedType)

module File = struct
  type t = string (* string --> file .ml ??*)

  let compare = String.compare
end

module FileRepl = Map.Make (File)
include OrderedType

let repls = ref FileRepl.empty

let add fname r =
  let frepls =
    match FileRepl.find_opt fname !repls with
    | Some rs -> Set.add r rs
    | None -> Set.singleton r
  in
  repls := FileRepl.add fname frepls !repls
;;

let mk loc p = { location = loc; payload = p }
let location { location; _ } = location

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

let apply_all repls fcontent =
  let flines = Array.of_list (String.split_on_char '\n' fcontent) in
  let cur = ref { dummy_pos with pos_lnum = 1; pos_cnum = 0; pos_bol = 0 } in
  let apply_repl { location = { loc_start; loc_end; _ } as loc; payload } buf =
    if check_loc loc flines
    then (
      let buf = payload_between_repls (!cur, loc_start) flines buf in
      let () =
        match payload with
        | Void -> ()
        | Space_padding -> Buffer.add_string buf (space_padding loc flines)
        | Padding p -> Buffer.add_string buf p
      in
      cur := loc_end)
    else
      (
      print_string
      @@ Printf.sprintf
           "damn. Maybe lint recognized a false constr. file: %s line_st: %d col_st: %d \
            line_end: %d col_end:%d\n"
           loc_start.pos_fname
           loc_start.pos_lnum
           (loc_start.pos_cnum - loc_start.pos_bol)
           loc_end.pos_lnum
           (loc_end.pos_cnum - loc_end.pos_bol));
    buf
  in
  let buf = Buffer.create (String.length fcontent) in
  let buf = Set.fold (fun repl buf -> apply_repl repl buf) repls buf in
  let file_end =
    { pos_lnum = Array.length flines
    ; pos_cnum = String.length flines.(Array.length flines - 1)
    ; pos_bol = 0
    ; pos_fname = !cur.pos_fname
    }
  in
  let buf = payload_between_repls (!cur, file_end) flines buf in
  Buffer.contents buf
;;

let get_gen_rule f fgen =
  let open Pervasives in
  let oc = open_out_gen [ Open_append; Open_creat ] 0o666 "fix_gen/get_diff.sh" in
  Printf.fprintf oc "diff %s %s;\n" f fgen;
  (* заставить показывать только файлы, где произошли изменения*)
  close_out oc;
  let oc = open_out_gen [ Open_append; Open_creat ] 0o666 "fix_gen/promote.sh" in
  Printf.fprintf oc "cat %s > %s;\n" fgen f;
  close_out oc
;;

(*[TEMPORARY] we plan use a combination if dune diff and promote (maybe git variant) for user preview*)
let apply_all _ =
  let _status =
    Sys.command "mkdir fix_gen; echo > fix_gen/get_diff.sh; echo > fix_gen/promote.sh"
  in
  let file_content fname = In_channel.with_open_text fname In_channel.input_all in
  let new_payloads =
    FileRepl.fold
      (fun fname frepls fr_acc ->
        (fname, apply_all frepls (file_content fname)) :: fr_acc)
      !repls
      []
  in
  let acc =
    List.fold_left
      (fun acc (fpath, payload) ->
        let pcs = String.split_on_char '/' fpath in
        let fpath_cor = List.nth pcs (List.length pcs - 1) ^ ".corrected" in
        let fpath_cor = "fix_gen/" ^ fpath_cor in
        let oc = open_out fpath_cor in
        Printf.fprintf oc "%s" payload;
        close_out oc;
        get_gen_rule fpath fpath_cor;
        acc + 1)
      0
      new_payloads
  in
  let _status = Sys.command "./fix_gen/get_diff.sh" in
  match _status = acc with
  | true -> print_string "wow\n"
  | _ -> ()
;;
