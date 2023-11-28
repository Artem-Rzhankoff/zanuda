(** Copyright 2021-2023, Kakadu. *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Location
open Lexing
open Padding

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
    { location = { loc_start = { pos_cnum = sc; _ }; loc_end = { pos_cnum = ec; _ }; _ }
    ; _
    }
    { location = { loc_start = { pos_cnum = sc'; _ }; loc_end = { pos_cnum = ec'; _ }; _ }
    ; _
    }
    =
    if sc = sc' then ec - ec' else sc - sc'
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
(* сюда надо только передавать координаты старой конструкции *)
(* добавить кейс, где линтер неправильно срабатывает*)

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
      print_string
      @@ Printf.sprintf
           "damn. Maybe lint recognized a false constr. file: %s line_st: %d col_st: %d \
            line_end: %d col_end:%d\n"
           loc_start.pos_fname
           loc_start.pos_lnum
           (loc_start.pos_cnum - loc_start.pos_bol)
           loc_end.pos_lnum
           (loc_end.pos_cnum - loc_end.pos_bol);
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

open Log

let apply_all _ =
  create_promote_script;
  let new_payloads =
    FileRepl.fold
      (fun fname frepls fr_acc ->
        (fname, apply_all frepls (file_content fname)) :: fr_acc)
      !repls
      []
  in
  List.iter
    (fun (file, payload) ->
      let corrected_file = name_corrected_file file in
      gen_corrected_file corrected_file payload;
      diff_log file corrected_file)
    new_payloads
;;
