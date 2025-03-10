(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type binop =
  | Mul
  | Div
  | Add
  | Sub
  | Eq
  | Neq
  | Lt
  | Gt
  | And
  | Or

type unop = Not

type immediate =
  | Avar of ident
  | Aconst of const

type cexpr =
  | CImm of immediate
  | CBinop of binop * immediate * immediate
  | CUnop of unop * immediate
  | CTuple of immediate list
  | CGetfield of int * immediate (* tuple or list access *)
  | CCons of immediate * immediate
  | CFun of ident * aexpr
  | CApp of immediate * immediate list
  | CIte of immediate * aexpr * aexpr

and aexpr =
  | ALet of rec_flag * ident * cexpr * aexpr
  | AExpr of cexpr

type astructure_item = AStr_value of rec_flag * (ident * aexpr) list
type astructure = astructure_item list
type arities = (string, int, Base.String.comparator_witness) Base.Map.t

val transform : structure -> arities * astructure
