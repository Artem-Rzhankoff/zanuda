open Base
open Lexing
open Warnings

class ['self] result_iter =
  object
    inherit ['self] Typedtree_visitor.iter
    method! visit_array _f _env _xs = ()
    (* тут наверное определить visit_expression, где будут находить выражение по локе*)
  end

let a = if true then true else false

module type REFACTORING = sig
  val visitor
    : < visit_Closed : Location.t -> _
      ; visit_tt_case :
          'a.
          (Location.t -> 'a -> unit) -> Location.t -> 'a Typedtree_visitor.tt_case -> unit
      ; .. >
        result_iter
end

module rec IfBool : sig
  include module type of struct
    let visitor
      : < visit_Closed : Location.t -> _
        ; visit_tt_case :
            'a.
            (Location.t -> 'a -> unit)
            -> Location.t
            -> 'a Typedtree_visitor.tt_case
            -> unit
        ; .. >
      =
      object (_self)
        inherit [_] result_iter
      end
    ;;
  end
end = struct
  let visitor =
    object (_self)
      inherit [_] result_iter as super

      method! visit_expression
        env
        ({ exp_env = _exp_env; exp_loc; exp_desc; exp_extra = _exp_extra; _ } as this) =
        (match exp_desc with
         | Texp_apply (f, args) ->
           let pat =
             let ops =
               let open Tast_pattern in
               texp_apply2 (texp_ident (path [ "Stdlib"; "&&" ])) ebool drop
               ||| texp_apply2 (texp_ident (path [ "Stdlib"; "&&" ])) drop ebool
               |> map1 ~f:(fun _ -> Format.asprintf "Conjunction with boolean smells bad")
             in
             ops
           in
           Tast_pattern.parse
             pat
             this.exp_loc
             ~on_error:(fun _desc () -> ())
             this
             (fun s () ->
               print_string
               @@ Printf.sprintf
                    "apply file: %s line: %d col: %d\n"
                    (* ну вместо печати надо будет вызывать replace модуль с соответствующим методом *)
                    this.exp_loc.loc_start.pos_fname
                    this.exp_loc.loc_start.pos_lnum
                    (this.exp_loc.loc_start.pos_cnum - this.exp_loc.loc_start.pos_bol))
             ()
         | Texp_ifthenelse (_ie, _te, _ee) ->
           let pat =
             let open Tast_pattern in
             let ite =
               texp_ite ebool drop drop
               |> map1 ~f:(Format.asprintf "Executing 'if %b' smells bad")
               ||| (texp_ite drop ebool drop
                    |> map1 ~f:(Format.asprintf "Executing 'if ... then %b' smells bad"))
               ||| (texp_ite drop drop (some ebool)
                    |> map1
                         ~f:
                           (Format.asprintf
                              "Executing 'if ... then .. else %b' smells bad"))
             in
             ite
           in
           Tast_pattern.parse
             pat
             this.exp_loc
             ~on_error:(fun _desc () -> ())
             this
             (fun s () ->
               print_string
               @@ Printf.sprintf
                    "if_bool file: %s line: %d col: %d\n"
                    exp_loc.loc_start.pos_fname
                    exp_loc.loc_start.pos_lnum
                    (exp_loc.loc_start.pos_cnum - exp_loc.loc_start.pos_bol))
             ()
         | _ -> ());
        super#visit_expression env this
    end
  ;;
end

let no_ident ident c =
  let exception Found in
  let open Typedtree in
  let visitor
    : < visit_Closed : Ident.t -> _
      ; visit_tt_case :
          'a. (Ident.t -> 'a -> unit) -> Ident.t -> 'a Typedtree_visitor.tt_case -> unit
      ; .. >
    =
    object (_self)
      inherit [_] result_iter as super

      method! visit_expression
        ident
        ({ exp_env = _exp_env; exp_loc = _exp_loc; exp_desc; exp_extra = _exp_extra; _ }
         as this) =
        match exp_desc with
        | Texp_ident (Path.Pident id, _, _) when Ident.equal id ident -> raise Found
        | Texp_function { param } when Ident.equal ident param -> ()
        | _ -> super#visit_expression ident this

      method! visit_tt_case
        : 'a. (Ident.t -> 'a -> unit) -> Ident.t -> 'a Typedtree_visitor.tt_case -> unit =
        fun visit_'a ident this ->
          let _id_name = Ident.name ident in
          (* the purpose of this is that otherwise the method type becomes monomorphic *)
          match Ident.name ident with
          | _id_name ->
            (match c.c_lhs.pat_desc with
             | Tpat_value v ->
               (match (v :> pattern) with
                | { pat_desc = Tpat_var (id, _) } ->
                  if Ident.equal ident id
                  then ()
                  else
                    super#visit_tt_case
                      visit_'a
                      ident
                      this 
                | _ -> super#visit_tt_case visit_'a ident this)
             | _ -> super#visit_tt_case visit_'a ident this)
    end
  in
  try
    visitor#visit_tt_case (fun _ _ -> ()) ident c;
    true
  with
  | Found -> false
;;

module rec ProposeFunction : sig
  include module type of struct
    let visitor
      : < visit_Closed : Location.t -> _
        ; visit_tt_case :
            'a.
            (Location.t -> 'a -> unit)
            -> Location.t
            -> 'a Typedtree_visitor.tt_case
            -> unit
        ; .. >
      =
      object (_self)
        inherit [_] result_iter
      end
    ;;
  end
end = struct
  let visitor =
    object (_self)
      inherit [_] result_iter as super

      method! visit_expression
        env
        ({ exp_env = _exp_env; exp_loc; exp_desc; exp_extra = _exp_extra; _ } as this) =
        match exp_desc with
        | Texp_function { arg_label = _arg_label; param = _param; cases = _cases; partial = _partial }->
          let pat =
            let open Tast_pattern in
            texp_function (case (tpat_var __) none (texp_match (texp_ident __) __) ^:: nil)
          in
          Tast_pattern.parse
            pat
            exp_loc
            ~on_error:(fun _desc () -> ())
            this
            (fun argname ident cases () ->
              match ident with
              | Path.Pident id ->
                if String.equal argname (Ident.name id)
                   && List.for_all cases ~f:(no_ident id)
                then
                  print_string
                  @@ Printf.sprintf
                       "propose_function file: %s line: %d col: %d\n"
                       exp_loc.loc_start.pos_fname
                       exp_loc.loc_start.pos_lnum
                       (exp_loc.loc_start.pos_cnum - exp_loc.loc_start.pos_bol)
              | _ -> ())
            ()
        | _ ->
          ();
          super#visit_expression env this
    end
  ;;
end
