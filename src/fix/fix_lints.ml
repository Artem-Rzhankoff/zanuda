open Parse
open Lexing
open Base
open Warnings

open Typedtree
open Zanuda_core

let get_code { loc_start; loc_end; _ } =
  let st_l, st_c = loc_start.pos_lnum, loc_start.pos_cnum in
  let fn_l, fn_c = loc_end.pos_lnum, loc_end.pos_cnum in
  let contents = Arg.read_arg loc_start.pos_fname in
  let st_str, fn_str = Array.get contents (st_l - 1), Array.get contents (fn_l - 1) in
  let start = String.sub ~pos:st_c ~len:(String.length st_str) st_str in
  let finish = String.sub ~pos:0 ~len:fn_c fn_str in
  let between_str =
    Array.fold
      (Array.sub contents ~pos:(st_l + 1) ~len:(fn_l - st_l - 1))
      ~init:""
      ~f:(fun acc el -> String.append acc (String.append el "\n"))
  in
  start ^ between_str ^ finish
;;

open Dune_project

let fine_module { impl } =
  match impl with
  | Some s when String.is_suffix s ~suffix:".ml-gen" -> false
  | _ -> true
;;

(* сохранять ли локи линтов просто во время работы линтера или заново добывать их при вызове фикса ??
   то есть cmt files уже можно сказать, что есть
   я бы сначала запускал зануду (все линты), но без распечатывания результата, а потом уже вот этот fix
   узнать, что будет, если сначал вызывали линтер обычной командой, а потом вызывем фикс без изменения директории
   будут запускаться все линты или есть какое-то хэширование??
*)
open LoadDune

(* loc надо будет заменить на то, что действительно понадобится в дальнейшем, чтоб лишнее не гонять *)
let find_correspond_cmt filenames =
  let s =
    let ch = Unix.open_process_in "dune describe" in
    let s = Sexplib.Sexp.input_sexp ch in
    Caml.close_in ch;
    s
  in
  let db = [%of_sexp: t list] s in
  let get_library name =
    List.find_map db ~f:(function
      | Library l when String.equal name l.uid -> Some l
      | _ -> None)
  in
  let on_module _ m found =
    List.fold
      [ m.impl, m.cmt; m.intf, m.cmti ]
      ~f:(fun acc ->
        function
        | Some source_filename, Some cmt_filename ->
          List.fold ~init:acc filenames ~f:(fun acc1 f ->
            if String.is_suffix ~suffix:f.loc_start.pos_fname source_filename
            then List.cons (cmt_filename, f) acc1
            else acc1)
        | _ -> acc)
      ~init:found
  in
  let loop_database () =
    List.fold ~init:[] db ~f:(fun acc ->
        function
        | Executables { modules; requires } | Library { Library.modules; requires } ->
          let extra_paths =
            requires
            |> List.filter_map ~f:(fun uid -> get_library uid)
            |> List.concat_map ~f:(fun { Library.include_dirs } -> include_dirs)
          in
          List.fold ~init:acc modules ~f:(fun acc1 m ->
            if fine_module m then on_module extra_paths m acc1 else acc1)
          (* за что отвечает fine_module ?? *)
        | _ -> acc)
  in
  List.rev @@ loop_database ()
;;

open Tast_pattern

(* по локе находим соответствующую конструкцию и дальше по какой-то базе будем это преображать *)

let find_by_loc (cmt_filename, (src_loc : Location.t)) =
  let cmt = Cmt_format.read_cmt cmt_filename in
  (* List.iter cmt.cmt_comments ~f:(fun (s, loc) -> print_string @@ Printf.sprintf "wtf string:%s  loc:%d\n" s loc.loc_start.pos_lnum ) *)
  (* сейчас надо понять, что передается в качетсве loc в tast_pattern и есть ли у меня сейчас шанс юзануть эту штучку *)
  match cmt.Cmt_format.cmt_annots with
  | Cmt_format.Implementation stru ->
    Refactoring.IfBool.visitor#visit_structure
      src_loc
      stru
  | Cmt_format.Interface sign ->
    Refactoring.IfBool.visitor#visit_signature
    src_loc
    sign
  | _ -> ()
;;

let foo ~untyped:analyze_untyped ~cmt:analyze_cmt ~cmti:analyze_cmti path =
  analyze_dir ~untyped:analyze_untyped ~cmt:analyze_cmt ~cmti:analyze_cmti path; (* запускаем поиск всех линтов *)
  let loc_lints = CollectedLints.loc_lints (fun (loc, _) -> loc) in
  (* если a это ссылка на loc_lints, то почему при изменении a не меняется loc_lints *)
  Queue.filter_inplace ~f:(fun loc -> loc != Location.none) loc_lints;
  let loc_lints = Base.Queue.to_list loc_lints in
  let plz = find_correspond_cmt loc_lints in
  (* let plz =
    List.filter plz ~f:(fun (f, loc) ->
      String.is_prefix ~prefix: "review/diff_parser.ml" loc.loc_start.pos_fname)
  in
  *)
  List.iter plz ~f:(fun x -> find_by_loc x)
;;
