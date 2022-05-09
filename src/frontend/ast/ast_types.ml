open Base

type location = Lexing.position

let string_of_location location =
    Fmt.str "Line:%d Position:%d" location.Lexing.position_lnum
    (location.Lexing.position_cnum - location.Lexing.position_bol + 1)

module type ID = sig
    type t

    val of_string: string -> t
    val to_string: t -> string
    val ( = ): t -> t -> bool
end

module String_id = struct
    type t = string

    let of_string x = x
    let to_string x = x
    let ( = ) = String.( = )
end

module Id_name : ID = String_id
module Class_name : ID = String_id
module Field_name : ID = String_id

type recursivity  = Nonrecursive | Recursive

type access_level = Public | Private | Protected

type mutability   = Immutable | Mutable

type type_specifier =
  | TE_BOOL     of bool
  | TE_I32      of int32
  | TE_I64      of int64
  | TE_F64      of string
  | TE_CLASS    of Class_name.t * type_specifier option
  | TE_VOID
  | TE_CHAR     of char (* * Location.t * char option *)
  | TE_STRING   of string (* * Location.t * string option *)
  | TE_GENERIC

let rec string_of_type = function  
  | TE_BOOL   -> "bool"
  | TE_I32    -> "i32"
  | TE_I64    -> "i64"
  | TE_F64    -> "f64"
  | TE_VOID   -> "void"
  | TE_CHAR   -> "char"
  | TE_STRING -> "string"
  | TE_GENERIC -> "T"
  | TE_CLASS (class_name, opt_type_param) ->
      let opt_type_param_str =
          match opt_type_param with
          | Some type_param -> Fmt.str "<%s>" (string_of_type type_param)
          | None            -> "" in
     

(* Variable Labeling
type label = string *)

type arg_label =
    NoLabel
  | Labelled    of string (** label$T ... *)
  | Optional    of string (** ?label$T ... | ?T *)
  | KleeneStar  of string (** *label$T ... | *T *)
  | KleeneCross of string (** +label$T ... | +T *)

type 'a location = 'a Location.location = {
  txt: 'a;
  location: Location.t;
}

type bin_op =
  | BinOpADD          (* + *)
  | BinOpSUB          (* - *)
  | BinOpMUL          (* * *)
  | BinOpDIV          (* / *)
  | BinOpMOD          (* % *)
  | BinOpEXP          (* ** *)
  | BinOpLT           (* < *)
  | BinOpGT           (* > *)
  | BinOpLTEQ         (* <= *)
  | BinOpGTEQ         (* >= *)
  | BinOpISEQ         (* == *)
  | BinOpNOTEQ        (* != *)
  | BinOpLAND         (* && *)
  | BinOpLOR          (* || *)

type un_op =
  | UnOpPOS           (* + *)
  | UnOpNEG           (* - *)
  | UnOpNOT           (* ! *)

exception NotDesugaredGenericType of string