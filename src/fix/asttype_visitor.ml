type constant = Asttypes.constant =
    Const_int of int
  | Const_char of char
  | Const_string of string * Location_visitor.location_t * string option
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint

and rec_flag = Asttypes.rec_flag = Nonrecursive | Recursive

and direction_flag = Asttypes.direction_flag = Upto | Downto

(* Order matters, used in polymorphic comparison *)
and private_flag = Asttypes.private_flag = Private | Public

and mutable_flag = Asttypes.mutable_flag =  Immutable | Mutable

and virtual_flag = Asttypes.virtual_flag = Virtual | Concrete

and override_flag = Asttypes.override_flag = Override | Fresh

and closed_flag = Asttypes.closed_flag = Closed | Open

and label = string

and arg_label = Asttypes.arg_label =
    Nolabel
  | Labelled of string (** [label:T -> ...] *)
  | Optional of string (** [?label:T -> ...] *)

and 'a loc = 'a Location_visitor.loc = {
  txt : 'a;
  loc : Location_visitor.location_t;
} [@@name "ast_loc"]
and variance = Asttypes.variance =
  | Covariant
  | Contravariant
  | NoVariance

and injectivity = Asttypes.injectivity = 
  | Injective
  | NoInjectivity

[@@deriving visitors { variety = "iter"; polymorphic = true; monomorphic = ["'env"]; ancestors = ["Location_visitor.iter"]; nude = true}]
