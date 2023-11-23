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

let available_lints =
  let open Refactoring in
  [ (module IfBool.Lint : Lint_refactoring.REFACTORING); (module ProposeFunction.Lint : Lint_refactoring.REFACTORING) ]
;;

open LoadDune

let find_correspond_cmt filenames =
  let s =
    let ch = Unix.open_process_in "dune describe" in
    let s = Sexplib.Sexp.input_sexp ch in
    Caml.close_in ch;
    s
  in
  let db = [%of_sexp: t list] s in
  let on_module m found =
    List.fold
      [ m.impl, m.cmt; m.intf, m.cmti ]
      ~f:(fun acc ->
        function
        | Some source_filename, Some cmt_filename ->
          let lint_loc =
            List.find filenames ~f:(fun f ->
              String.is_suffix ~suffix:f.loc_start.pos_fname source_filename)
          in
          (match lint_loc with
           | Some loc -> List.cons (cmt_filename, loc) acc
           | _ -> acc)
        | _ -> acc)
      ~init:found
  in
  let loop_database () =
    List.fold ~init:[] db ~f:(fun acc ->
        function
        | Executables { modules; _ } | Library { Library.modules; _ } ->
          (* просто вот тут надо все модули разом передавать *)
          List.fold ~init:acc modules ~f:(fun acc1 m ->
            if fine_module m then on_module m acc1 else acc1)
        | _ -> acc)
  in
  List.rev @@ loop_database ()
;;

open Tast_pattern

let find_by_loc (cmt_filename, (src_loc : Location.t)) =
  let open Refactoring in
  let cmt = Cmt_format.read_cmt cmt_filename in
  match cmt.Cmt_format.cmt_annots with
  | Cmt_format.Implementation stru ->
    List.iter available_lints ~f:(fun (module L : Lint_refactoring.REFACTORING) ->
      L.visitor#visit_structure src_loc stru)
  | Cmt_format.Interface sign ->
    List.iter available_lints ~f:(fun (module L : Lint_refactoring.REFACTORING) ->
      L.visitor#visit_signature src_loc sign)
  | _ -> ()
;;

let foo ~untyped:analyze_untyped ~cmt:analyze_cmt ~cmti:analyze_cmti path =
  analyze_dir ~untyped:analyze_untyped ~cmt:analyze_cmt ~cmti:analyze_cmti path;
  let loc_lints = CollectedLints.loc_lints (fun (loc, _) -> loc) in
  Queue.filter_inplace ~f:(fun loc -> loc != Location.none) loc_lints;
  let loc_lints = Base.Queue.to_list loc_lints in
  let plz = find_correspond_cmt loc_lints in
  let plz =
     List.filter plz ~f:(fun (f, loc) ->
     String.is_prefix ~prefix: "foo/main.ml" loc.loc_start.pos_fname)
  in
  let acc = List.fold_left plz ~f:(fun acc x -> find_by_loc x; ( (+) acc 1)) ~init:0 in
  Replacement.Repl.apply_all acc
;;

