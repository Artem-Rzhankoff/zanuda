(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Representation of types and declarations *)

open Asttypes
module Uid = Shape.Uid
module Base = Types

(* Maps of methods and instance variables *)

module MethSet = Misc.Stdlib.String.Set
module VarSet = Misc.Stdlib.String.Set
module Meths = Misc.Stdlib.String.Map
module Vars = Misc.Stdlib.String.Map

type ty_transient_expr = {
  mutable desc : ty_type_desc;
  mutable level : int;
  mutable scope : int;
  id : int;
}

and ty_type_expr = ty_transient_expr

and ty_type_desc = 
  | Tvar of string option
  | Tarrow of arg_label * ty_type_expr * ty_type_expr * ty_commutable
  | Ttuple of ty_type_expr list
  | Tconstr of Path_visitor.path_t * ty_type_expr list * ty_abbrev_memo ref
  | Tobject of ty_type_expr * (Path_visitor.path_t * ty_type_expr list) option ref
  | Tfield of string * ty_field_kind * ty_type_expr * ty_type_expr
  | Tnil
  | Tlink of ty_type_expr
  | Tsubst of ty_type_expr * ty_type_expr option
  | Tvariant of ty_row_desc
  | Tunivar of string option
  | Tpoly of ty_type_expr * ty_type_expr list
  | Tpackage of
      Path_visitor.path_t * (Longident_visitor.longident_t * ty_type_expr) list

and ty_row_desc = {  (*??*)
  row_fields : (label * ty_row_field) list;
  row_more : ty_type_expr;
  row_closed : bool;
  row_fixed : ty_fixed_explanation option;
  row_name : (Path_visitor.path_t * ty_type_expr list) option;
}

and ty_fixed_explanation = 
  | Univar of ty_type_expr
  | Fixed_private
  | Reified of Path_visitor.path_t
  | Rigid

and ty_row_field = ty_row_field_gen [@opaque]
(* and ty_row_field = [ `some ] ty_row_field_gen [@opaque] *)
(* and 'a ty_row_field_gen = *)
and ty_row_field_gen = 
  | RFpresent of ty_type_expr option (*: ty_type_expr option -> [> `some ] ty_row_field_gen *)
  | RFeither of bool * ty_type_expr list * bool * ty_row_field option ref
  (*| RFeither : {
      no_arg : bool;
      arg_type : ty_type_expr list;
      matched : bool;
      ext : ([ `some | `none ] ty_row_field_gen[@opaque]) ref;
    }
      -> ([> `some ] ty_row_field_gen [@opaque]) *)
  | RFabsent (* : ([> `some ] ty_row_field_gen [@opaque])*)
  | RFnone (* : ([> `none ] ty_row_field_gen [@opaque])*)

and ty_abbrev_memo = 
  | Mnil
  | Mcons of
      private_flag * Path_visitor.path_t * ty_type_expr * ty_type_expr * ty_abbrev_memo
  | Mlink of ty_abbrev_memo ref

(* and ty_any = [ `some | `none | `var ] [@opaque] *)
and ty_field_kind = ty_field_kind_gen

(* and 'a ty_field_kind_gen = *)
and ty_field_kind_gen =
  | FKvar (* : {
    mutable ty_field_kind : ty_any ty_field_kind_gen;
  }
    -> [> `var ] ty_field_kind_gen*)
  | FKprivate (* : [> `none ] ty_field_kind_gen*)
  | FKpublic (* : [> `some ] ty_field_kind_gen *)
  | FKabsent (* : [> `some ] ty_field_kind_gen *)

and ty_commutable = ty_commutable_gen (*??*)
(* and ty_commutable = ([ `some | `var ] [@opaque]) ty_commutable_gen (*??*) *)
(* and 'a ty_commutable_gen =*)
and ty_commutable_gen = (* вот тут какой-то корень зол*)
  | Cok (* : ([> `some ] [@opaque]) ty_commutable_gen *)
  | Cunknown (* : ([> `none ] [@opaque]) ty_commutable_gen *)
  | Cvar (* : { mutable commu : ty_any ty_commutable_gen } -> ([> `var ] [@opaque]) ty_commutable_gen*)

and ty_value_description = {
  val_type : ty_type_expr; (* Type of the value *)
  val_kind : ty_value_kind;
  val_loc : Location_visitor.location_t;
  val_attributes : (Parsetree.attributes[@opaque]);
  val_uid : (Uid.t[@opaque]);
}

and ty_value_kind =
  | Val_reg (* Regular value *)
  | Val_prim of (Primitive.description[@opaque]) (* Primitive *)
  | Val_ivar of mutable_flag * string (* Instance variable (mutable ?) *)
  | Val_self of
      ty_class_signature
      * ty_self_meths
      * (Ident_visitor.ident_t Vars.t [@opaque])
      * string
    (* Self *)
  | Val_anc of
      ty_class_signature * (Ident_visitor.ident_t Meths.t[@opaque]) * string
(* Ancestor *)

and ty_self_meths =
  | Self_concrete of (Ident_visitor.ident_t Meths.t [@opaque]) (*??*)
  | Self_virtual of (Ident_visitor.ident_t Meths.t  ref [@opaque])

and ty_class_signature = {
  csig_self : ty_type_expr;
  mutable csig_self_row : ty_type_expr;
  mutable csig_vars :
    ((mutable_flag * virtual_flag * ty_type_expr) Vars.t[@opaque]);
  mutable csig_meths :
    ((ty_method_privacy * virtual_flag * ty_type_expr) Meths.t[@opaque]);
}

and ty_method_privacy = Mpublic | Mprivate of ty_field_kind (*??*)
and ty_variance_t = (Base.Variance.t [@opaque])

and ty_type_declaration = {
  type_params : ty_type_expr list;
  type_arity : int;
  ty_type_kind : ty_type_decl_kind;
  type_private : private_flag;
  type_manifest : ty_type_expr option;
  type_variance : ty_variance_t list; (* Variance.t*)
  type_separability : variance list; (* Separapility.t*)
  type_is_newtype : bool;
  type_expansion_scope : int;
  type_loc : Location_visitor.location_t;
  type_attributes : (Parsetree.attributes[@opaque]);
  type_immediate : (Type_immediacy.t[@opaque]);
  type_unboxed_default : bool;
  type_uid : (Uid.t[@opaque]);
}

and ty_type_decl_kind = (ty_label_declaration, ty_constructor_declaration) ty_type_kind

and ('lbl, 'cstr) ty_type_kind = (*??*)
  | Type_abstract of ty_type_origin
  | Type_record of 'lbl list * ty_record_representation
  | Type_variant of 'cstr list * ty_variant_representation
  | Type_open

and ty_type_origin = Definition | Rec_check_regularity | Existential of string (*??*)

and ty_record_representation =
  | Record_regular (* All fields are boxed / tagged *)
  | Record_float (* All fields are floats *)
  | Record_unboxed of bool (* Unboxed single-field record, inlined or not *)
  | Record_inlined of int (* Inlined record *)
  | Record_extension of Path_visitor.path_t (* Inlined record under extension *)

and ty_variant_representation =
  | Variant_regular (* Constant or boxed constructors *)
  | Variant_unboxed (* One unboxed single-field constructor *)

and ty_label_declaration = {
  ld_id : Ident_visitor.ident_t;
  ld_mutable : mutable_flag;
  ld_type : ty_type_expr;
  ld_loc : Location_visitor.location_t;
  ld_attributes : (Parsetree.attributes[@opaque]);
  ld_uid : (Uid.t[@opaque]);
}

and ty_constructor_declaration = {
  cd_id : Ident_visitor.ident_t;
  cd_args : ty_constructor_arguments;
  cd_res : ty_type_expr option;
  cd_loc : Location_visitor.location_t;
  cd_attributes : (Parsetree.attributes[@opaque]);
  cd_uid : (Uid.t[@opaque]);
}

and ty_constructor_arguments =
  | Cstr_tuple of ty_type_expr list
    [@name "ty_Cstr_tuple"]
  | Cstr_record of ty_label_declaration list
    [@name "ty_Cstr_record"]

and ty_extension_constructor = {
  ext_type_path : Path_visitor.path_t;
  ext_type_params : ty_type_expr list;
  ext_args : ty_constructor_arguments;
  ext_ret_type : ty_type_expr option;
  ext_private : private_flag;
  ext_loc : Location_visitor.location_t;
  ext_attributes : (Parsetree.attributes[@opaque]);
  ext_uid : (Uid.t[@opaque]);
}
(*
and type_transparence =
  | Type_public (* unrestricted expansion *)
  | Type_new (* "new" and *)
  | Type_private (* private and *)
*)
(* This type is not referred to by anything else. *)
(*??*)
and ty_class_type =
  | Cty_constr of Path_visitor.path_t * ty_type_expr list * ty_class_type
  | Cty_signature of ty_class_signature
  | Cty_arrow of arg_label * ty_type_expr * ty_class_type

and ty_class_declaration = {
  cty_params : ty_type_expr list;
  mutable cty_type : ty_class_type;
  cty_path : Path_visitor.path_t;
  cty_new : ty_type_expr option;
  cty_variance : ty_variance_t list; (* Variance.t *)
  cty_loc : Location_visitor.location_t;
  cty_attributes : (Parsetree.attributes[@opaque]);
  cty_uid : (Uid.t[@opaque]);
}

and ty_class_type_declaration = {
  clty_params : ty_type_expr list;
  clty_type : ty_class_type;
  clty_path : Path_visitor.path_t;
  clty_hash_type : ty_type_declaration;
  clty_variance : ty_variance_t list; (* Variance.t *)
  clty_loc : Location_visitor.location_t;
  clty_attributes : (Parsetree.attributes[@opaque]);
  clty_uid : (Uid.t[@opaque]);
}

(* Type expressions for the module language *)
and ty_visibility = Base.visibility = Exported | Hidden

and ty_module_type =
  | Mty_ident of Path_visitor.path_t
  | Mty_signature of ty_signature
  | Mty_functor of ty_functor_parameter * ty_module_type
  | Mty_alias of Path_visitor.path_t

and ty_functor_parameter = (*??*)
  | Unit
    [@name "ty_Unit"]
  | Named of Ident_visitor.ident_t option * ty_module_type
    [@name "ty_Named"]

and ty_module_presence = Base.module_presence = Mp_present | Mp_absent

and ty_signature = ty_signature_item list

and ty_signature_item =
  | Sig_value of Ident_visitor.ident_t * ty_value_description * ty_visibility
  | Sig_type of
      Ident_visitor.ident_t * ty_type_declaration * ty_rec_status * ty_visibility
  | Sig_typext of
      Ident_visitor.ident_t * ty_extension_constructor * ty_ext_status * ty_visibility
  | Sig_module of
      Ident_visitor.ident_t
      * ty_module_presence
      * ty_module_declaration
      * ty_rec_status
      * ty_visibility
  | Sig_modtype of Ident_visitor.ident_t * ty_modtype_declaration * ty_visibility
  | Sig_class of
      Ident_visitor.ident_t * ty_class_declaration * ty_rec_status * ty_visibility
  | Sig_class_type of
      Ident_visitor.ident_t * ty_class_type_declaration * ty_rec_status * ty_visibility

and ty_module_declaration = {
  md_type : ty_module_type;
  md_attributes : (Parsetree.attributes[@opaque]);
  md_loc : Location_visitor.location_t;
  md_uid : (Uid.t[@opaque]);
}

and ty_modtype_declaration = {
  mtd_type : ty_module_type option; (* Note: abstract *)
  mtd_attributes : (Parsetree.attributes[@opaque]);
  mtd_loc : Location_visitor.location_t;
  mtd_uid : (Uid.t[@opaque]);
}

and ty_rec_status = Base.rec_status = 
  | Trec_not (* first in a nonrecursive group *)
  | Trec_first (* first in a recursive group *)
  | Trec_next (* not first in a recursive/nonrecursive group *)

and ty_ext_status = Base.ext_status = 
  | Text_first (* first constructor of an extension *)
  | Text_next (* not first constructor of an extension *)
  | Text_exception (* an exception *)

and ty_constructor_description = {
  cstr_name : string; (* Constructor name *)
  cstr_res : ty_type_expr; (* Type of the result *)
  cstr_existentials : ty_type_expr list; (* list of existentials *)
  cstr_args : ty_type_expr list; (* Type of the arguments *)
  cstr_arity : int; (* Number of arguments *)
  cstr_tag : ty_constructor_tag; (* Tag for heap blocks *)
  cstr_consts : int; (* Number of constant constructors *)
  cstr_nonconsts : int; (* Number of non-const constructors *)
  cstr_generalized : bool; (* Constrained return and? *)
  cstr_private : private_flag; (* Read-only constructor? *)
  cstr_loc : Location_visitor.location_t;
  cstr_attributes : (Parsetree.attributes[@opaque]);
  cstr_inlined : ty_type_declaration option;
  cstr_uid : (Uid.t[@opaque]); 
}

and ty_constructor_tag = Base.constructor_tag = 
  | Cstr_constant of int (* Constant constructor (an int) *)
  | Cstr_block of int (* Regular constructor (a block) *)
  | Cstr_unboxed (* Constructor of an unboxed and *)
  | Cstr_extension of Path_visitor.path_t * bool
(* Extension constructor
   true if a constant false if a block*)

and ty_label_description = {
  lbl_name : string; (* Short name *)
  lbl_res : ty_type_expr; (* Type of the result *)
  lbl_arg : ty_type_expr; (* Type of the argument *)
  lbl_mut : mutable_flag; (* Is this a mutable field? *)
  lbl_pos : int; (* Position in block *)
  lbl_all : ty_label_description array; (* All the labels in this and *)
  lbl_repres : ty_record_representation; (* Representation for this record *)
  lbl_private : private_flag; (* Read-only field? *)
  lbl_loc : Location_visitor.location_t;
  lbl_attributes : (Parsetree.attributes[@opaque]);
  lbl_uid : (Uid.t[@opaque]);
}
(*
and change =
  | Ctype of ty_type_expr * ty_type_desc
  | Ccompress of ty_type_expr * ty_type_desc * ty_type_desc
  | Clevel of ty_type_expr * int
  | Cscope of ty_type_expr * int
  | Cname of
      (Path_visitor.path_t * ty_type_expr list) option ref
      * (Path_visitor.path_t * ty_type_expr list) option
  | Crow of ([ `none | `some ] [@opaque]) ty_row_field_gen ref
  | Ckind of ([ `var ] [@opaque]) ty_field_kind_gen
  | Ccommu of ([ `var ])  ty_commutable_gen
  | Cuniv of ty_type_expr option ref * ty_type_expr option

and changes = Change of change * changes ref | Unchanged | Invalid
and field_kind_view = Fprivate | Fpublic | Fabsent

and row_desc_repr =
  | Row of {
      fields : (label * ty_row_field) list;
      more : ty_type_expr;
      closed : bool;
      fixed : ty_fixed_explanation option;
      name : (Path_visitor.path_t * ty_type_expr list) option;
    }

and row_field_view =
  | Rpresent of ty_type_expr option
  | Reither of bool * ty_type_expr list * bool
  (* 1st true denotes a constant constructor *)
  (* 2nd true denotes a tag in a pattern matching, and
     is erased later *)
  | Rabsent
*)
[@@deriving
  visitors
    {
      variety = "iter";
      polymorphic = true;
      monomorphic = [ "'env";];
      ancestors =
        [
          "Asttype_visitor.iter"; "Path_visitor.iter"; "Longident_visitor.iter";
        ];
      nude = true;
    }]

(*
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Representation of types and declarations *)

open Asttypes
module Uid = Shape.Uid
module Base = Types

(* Maps of methods and instance variables *)

module MethSet = Misc.Stdlib.String.Set
module VarSet = Misc.Stdlib.String.Set
module Meths = Misc.Stdlib.String.Map
module Vars = Misc.Stdlib.String.Map

type ty_transient_expr = {
  mutable desc : ty_type_desc;
  mutable level : int;
  mutable scope : int;
  id : int;
}

and ty_type_expr = ty_transient_expr

and ty_type_desc =
  | Tvar of string option
  | Tarrow of arg_label * ty_type_expr * ty_type_expr * ty_commutable
  | Ttuple of ty_type_expr list
  | Tconstr of Path_visitor.path_t * ty_type_expr list * ty_abbrev_memo ref
  | Tobject of ty_type_expr * (Path_visitor.path_t * ty_type_expr list) option ref
  | Tfield of string * ty_field_kind * ty_type_expr * ty_type_expr
  | Tnil
  | Tlink of ty_type_expr
  | Tsubst of ty_type_expr * ty_type_expr option
  | Tvariant of ty_row_desc
  | Tunivar of string option
  | Tpoly of ty_type_expr * ty_type_expr list
  | Tpackage of
      Path_visitor.path_t * (Longident_visitor.longident_t * ty_type_expr) list

and ty_row_desc = {  (*??*)
  row_fields : (label * ty_row_field) list;
  row_more : ty_type_expr;
  row_closed : bool;
  row_fixed : ty_fixed_explanation option;
  row_name : (Path_visitor.path_t * ty_type_expr list) option;
}

and ty_fixed_explanation =
  | Univar of ty_type_expr
  | Fixed_private
  | Reified of Path_visitor.path_t
  | Rigid

and ty_row_field = ty_row_field_gen [@opaque]
(* and ty_row_field = [ `some ] ty_row_field_gen [@opaque] *)
(* and 'a ty_row_field_gen = *)
and ty_row_field_gen =
  | RFpresent of ty_type_expr option (*: ty_type_expr option -> [> `some ] ty_row_field_gen *)
  | RFeither of bool * ty_type_expr list * bool * ty_row_field option ref
  (*| RFeither : {
      no_arg : bool;
      arg_type : ty_type_expr list;
      matched : bool;
      ext : ([ `some | `none ] ty_row_field_gen[@opaque]) ref;
    }
      -> ([> `some ] ty_row_field_gen [@opaque]) *)
  | RFabsent (* : ([> `some ] ty_row_field_gen [@opaque])*)
  | RFnone (* : ([> `none ] ty_row_field_gen [@opaque])*)

and ty_abbrev_memo =
  | Mnil
  | Mcons of
      private_flag * Path_visitor.path_t * ty_type_expr * ty_type_expr * ty_abbrev_memo
  | Mlink of ty_abbrev_memo ref

(* and ty_any = [ `some | `none | `var ] [@opaque] *)
and ty_field_kind = ty_field_kind_gen

(* and 'a ty_field_kind_gen = *)
and ty_field_kind_gen =
  | FKvar (* : {
    mutable ty_field_kind : ty_any ty_field_kind_gen;
  }
    -> [> `var ] ty_field_kind_gen*)
  | FKprivate (* : [> `none ] ty_field_kind_gen*)
  | FKpublic (* : [> `some ] ty_field_kind_gen *)
  | FKabsent (* : [> `some ] ty_field_kind_gen *)

and ty_commutable = ty_commutable_gen (*??*)
(* and ty_commutable = ([ `some | `var ] [@opaque]) ty_commutable_gen (*??*) *)
(* and 'a ty_commutable_gen =*)
and ty_commutable_gen = (* вот тут какой-то корень зол*)
  | Cok (* : ([> `some ] [@opaque]) ty_commutable_gen *)
  | Cunknown (* : ([> `none ] [@opaque]) ty_commutable_gen *)
  | Cvar (* : { mutable commu : ty_any ty_commutable_gen } -> ([> `var ] [@opaque]) ty_commutable_gen*)

and ty_value_description = {
  val_type : ty_type_expr; (* Type of the value *)
  val_kind : ty_value_kind;
  val_loc : Location_visitor.location_t;
  val_attributes : (Parsetree.attributes[@opaque]);
  val_uid : (Uid.t[@opaque]);
}

and ty_value_kind =
  | Val_reg (* Regular value *)
  | Val_prim of (Primitive.description[@opaque]) (* Primitive *)
  | Val_ivar of mutable_flag * string (* Instance variable (mutable ?) *)
  | Val_self of
      ty_class_signature
      * ty_self_meths
      * (Ident_visitor.ident_t Vars.t [@opaque])
      * string
    (* Self *)
  | Val_anc of
      ty_class_signature * (Ident_visitor.ident_t Meths.t[@opaque]) * string
(* Ancestor *)

and ty_self_meths =
  | Self_concrete of (Ident_visitor.ident_t Meths.t [@opaque]) (*??*)
  | Self_virtual of (Ident_visitor.ident_t Meths.t  ref [@opaque])

and ty_class_signature = {
  csig_self : ty_type_expr;
  mutable csig_self_row : ty_type_expr;
  mutable csig_vars :
    ((mutable_flag * virtual_flag * ty_type_expr) Vars.t[@opaque]);
  mutable csig_meths :
    ((ty_method_privacy * virtual_flag * ty_type_expr) Meths.t[@opaque]);
}

and ty_method_privacy = Mpublic | Mprivate of ty_field_kind (*??*)
and ty_variance_t = (Base.Variance.t [@opaque])

and ty_type_declaration = {
  type_params : ty_type_expr list;
  type_arity : int;
  ty_type_kind : ty_type_decl_kind;
  type_private : private_flag;
  type_manifest : ty_type_expr option;
  type_variance : ty_variance_t list; (* Variance.t*)
  type_separability : variance list; (* Separapility.t*)
  type_is_newtype : bool;
  type_expansion_scope : int;
  type_loc : Location_visitor.location_t;
  type_attributes : (Parsetree.attributes[@opaque]);
  type_immediate : (Type_immediacy.t[@opaque]);
  type_unboxed_default : bool;
  type_uid : (Uid.t[@opaque]);
}

and ty_type_decl_kind = (ty_label_declaration, ty_constructor_declaration) ty_type_kind

and ('lbl, 'cstr) ty_type_kind = (*??*)
  | Type_abstract of ty_type_origin
  | Type_record of 'lbl list * ty_record_representation
  | Type_variant of 'cstr list * ty_variant_representation
  | Type_open

and ty_type_origin = Definition | Rec_check_regularity | Existential of string (*??*)

and ty_record_representation =
  | Record_regular (* All fields are boxed / tagged *)
  | Record_float (* All fields are floats *)
  | Record_unboxed of bool (* Unboxed single-field record, inlined or not *)
  | Record_inlined of int (* Inlined record *)
  | Record_extension of Path_visitor.path_t (* Inlined record under extension *)

and ty_variant_representation =
  | Variant_regular (* Constant or boxed constructors *)
  | Variant_unboxed (* One unboxed single-field constructor *)

and ty_label_declaration = {
  ld_id : Ident_visitor.ident_t;
  ld_mutable : mutable_flag;
  ld_type : ty_type_expr;
  ld_loc : Location_visitor.location_t;
  ld_attributes : (Parsetree.attributes[@opaque]);
  ld_uid : (Uid.t[@opaque]);
}

and ty_constructor_declaration = {
  cd_id : Ident_visitor.ident_t;
  cd_args : ty_constructor_arguments;
  cd_res : ty_type_expr option;
  cd_loc : Location_visitor.location_t;
  cd_attributes : (Parsetree.attributes[@opaque]);
  cd_uid : (Uid.t[@opaque]);
}

and ty_constructor_arguments =
  | Cstr_tuple of ty_type_expr list
    [@name "ty_Cstr_tuple"]
  | Cstr_record of ty_label_declaration list
    [@name "ty_Cstr_record"]

and ty_extension_constructor = {
  ext_type_path : Path_visitor.path_t;
  ext_type_params : ty_type_expr list;
  ext_args : ty_constructor_arguments;
  ext_ret_type : ty_type_expr option;
  ext_private : private_flag;
  ext_loc : Location_visitor.location_t;
  ext_attributes : (Parsetree.attributes[@opaque]);
  ext_uid : (Uid.t[@opaque]);
}
(*
and type_transparence =
  | Type_public (* unrestricted expansion *)
  | Type_new (* "new" and *)
  | Type_private (* private and *)
*)
(* This type is not referred to by anything else. *)
(*??*)
and ty_class_type =
  | Cty_constr of Path_visitor.path_t * ty_type_expr list * ty_class_type
  | Cty_signature of ty_class_signature
  | Cty_arrow of arg_label * ty_type_expr * ty_class_type

and ty_class_declaration = {
  cty_params : ty_type_expr list;
  mutable cty_type : ty_class_type;
  cty_path : Path_visitor.path_t;
  cty_new : ty_type_expr option;
  cty_variance : ty_variance_t list; (* Variance.t *)
  cty_loc : Location_visitor.location_t;
  cty_attributes : (Parsetree.attributes[@opaque]);
  cty_uid : (Uid.t[@opaque]);
}

and ty_class_type_declaration = {
  clty_params : ty_type_expr list;
  clty_type : ty_class_type;
  clty_path : Path_visitor.path_t;
  clty_hash_type : ty_type_declaration;
  clty_variance : ty_variance_t list; (* Variance.t *)
  clty_loc : Location_visitor.location_t;
  clty_attributes : (Parsetree.attributes[@opaque]);
  clty_uid : (Uid.t[@opaque]);
}

(* Type expressions for the module language *)
and ty_visibility = Exported | Hidden

and ty_module_type =
  | Mty_ident of Path_visitor.path_t
  | Mty_signature of ty_signature
  | Mty_functor of ty_functor_parameter * ty_module_type
  | Mty_alias of Path_visitor.path_t

and ty_functor_parameter = (*??*)
  | Unit
    [@name "ty_Unit"]
  | Named of Ident_visitor.ident_t option * ty_module_type
    [@name "ty_Named"]

and ty_module_presence = Mp_present | Mp_absent

and ty_signature = ty_signature_item list

and ty_signature_item =
  | Sig_value of Ident_visitor.ident_t * ty_value_description * ty_visibility
  | Sig_type of
      Ident_visitor.ident_t * ty_type_declaration * ty_rec_status * ty_visibility
  | Sig_typext of
      Ident_visitor.ident_t * ty_extension_constructor * ty_ext_status * ty_visibility
  | Sig_module of
      Ident_visitor.ident_t
      * ty_module_presence
      * ty_module_declaration
      * ty_rec_status
      * ty_visibility
  | Sig_modtype of Ident_visitor.ident_t * ty_modtype_declaration * ty_visibility
  | Sig_class of
      Ident_visitor.ident_t * ty_class_declaration * ty_rec_status * ty_visibility
  | Sig_class_type of
      Ident_visitor.ident_t * ty_class_type_declaration * ty_rec_status * ty_visibility

and ty_module_declaration = {
  md_type : ty_module_type;
  md_attributes : (Parsetree.attributes[@opaque]);
  md_loc : Location_visitor.location_t;
  md_uid : (Uid.t[@opaque]);
}

and ty_modtype_declaration = {
  mtd_type : ty_module_type option; (* Note: abstract *)
  mtd_attributes : (Parsetree.attributes[@opaque]);
  mtd_loc : Location_visitor.location_t;
  mtd_uid : (Uid.t[@opaque]);
}

and ty_rec_status =
  | Trec_not (* first in a nonrecursive group *)
  | Trec_first (* first in a recursive group *)
  | Trec_next (* not first in a recursive/nonrecursive group *)

and ty_ext_status =
  | Text_first (* first constructor of an extension *)
  | Text_next (* not first constructor of an extension *)
  | Text_exception (* an exception *)

and ty_constructor_description = {
  cstr_name : string; (* Constructor name *)
  cstr_res : ty_type_expr; (* Type of the result *)
  cstr_existentials : ty_type_expr list; (* list of existentials *)
  cstr_args : ty_type_expr list; (* Type of the arguments *)
  cstr_arity : int; (* Number of arguments *)
  cstr_tag : ty_constructor_tag; (* Tag for heap blocks *)
  cstr_consts : int; (* Number of constant constructors *)
  cstr_nonconsts : int; (* Number of non-const constructors *)
  cstr_generalized : bool; (* Constrained return and? *)
  cstr_private : private_flag; (* Read-only constructor? *)
  cstr_loc : Location_visitor.location_t;
  cstr_attributes : (Parsetree.attributes[@opaque]);
  cstr_inlined : ty_type_declaration option;
  cstr_uid : (Uid.t[@opaque]);
}

and ty_constructor_tag =
  | Cstr_constant of int (* Constant constructor (an int) *)
  | Cstr_block of int (* Regular constructor (a block) *)
  | Cstr_unboxed (* Constructor of an unboxed and *)
  | Cstr_extension of Path_visitor.path_t * bool
(* Extension constructor
   true if a constant false if a block*)

and ty_label_description = {
  lbl_name : string; (* Short name *)
  lbl_res : ty_type_expr; (* Type of the result *)
  lbl_arg : ty_type_expr; (* Type of the argument *)
  lbl_mut : mutable_flag; (* Is this a mutable field? *)
  lbl_pos : int; (* Position in block *)
  lbl_all : ty_label_description array; (* All the labels in this and *)
  lbl_repres : ty_record_representation; (* Representation for this record *)
  lbl_private : private_flag; (* Read-only field? *)
  lbl_loc : Location_visitor.location_t;
  lbl_attributes : (Parsetree.attributes[@opaque]);
  lbl_uid : (Uid.t[@opaque]);
}
(*
and change =
  | Ctype of ty_type_expr * ty_type_desc
  | Ccompress of ty_type_expr * ty_type_desc * ty_type_desc
  | Clevel of ty_type_expr * int
  | Cscope of ty_type_expr * int
  | Cname of
      (Path_visitor.path_t * ty_type_expr list) option ref
      * (Path_visitor.path_t * ty_type_expr list) option
  | Crow of ([ `none | `some ] [@opaque]) ty_row_field_gen ref
  | Ckind of ([ `var ] [@opaque]) ty_field_kind_gen
  | Ccommu of ([ `var ])  ty_commutable_gen
  | Cuniv of ty_type_expr option ref * ty_type_expr option

and changes = Change of change * changes ref | Unchanged | Invalid
and field_kind_view = Fprivate | Fpublic | Fabsent

and row_desc_repr =
  | Row of {
      fields : (label * ty_row_field) list;
      more : ty_type_expr;
      closed : bool;
      fixed : ty_fixed_explanation option;
      name : (Path_visitor.path_t * ty_type_expr list) option;
    }

and row_field_view =
  | Rpresent of ty_type_expr option
  | Reither of bool * ty_type_expr list * bool
  (* 1st true denotes a constant constructor *)
  (* 2nd true denotes a tag in a pattern matching, and
     is erased later *)
  | Rabsent
*)
[@@deriving
  visitors
    {
      variety = "iter";
      polymorphic = true;
      monomorphic = [ "'env";];
      ancestors =
        [
          "Asttype_visitor.iter"; "Path_visitor.iter"; "Longident_visitor.iter";
        ];
      nude = true;
    }]
*)
