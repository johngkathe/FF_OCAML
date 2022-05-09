type location = Lexing.position

module type ID = sig
    type t

    val of_string: string -> t
    val to_string: t -> string
    val ( = ): t -> t -> bool
end

module Var_name : ID
module Class_name : ID
module Field_name : ID

type mutability = Immutable | Mutable

type type_specifier =
  | TE_BOOL
  | TE_I32
  | TE_I64
  | TE_F64
  | TE_CLASS    of Class_name.t * type_specifier option
  | TE_VOID
  | TE_CHAR     of char (* * Location.t * char option *)
  | TE_STRING   of string (* * Location.t * string option *)
  | TE_GENERIC

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