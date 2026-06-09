(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Eva_ast_types

(** Rewriting visitor *)

module Rewrite :
sig
  type visitor = {
    exp : exp -> exp;
    lval : lval -> lval;
    varinfo : varinfo -> varinfo;
    offset : offset -> offset;
  }

  type rewriter = {
    rewrite_exp : visitor:visitor -> exp -> exp;
    rewrite_lval : visitor:visitor -> lval -> lval;
    rewrite_varinfo : visitor:visitor -> varinfo -> varinfo;
    rewrite_offset : visitor:visitor -> offset -> offset;
  }

  val default : rewriter
  val visit_exp : rewriter -> exp -> exp
  val visit_lval : rewriter -> lval -> lval
end


(** Observing visitor *)

module Observe :
sig
  type 'a visitor = {
    neutral : 'a;
    combine : 'a -> 'a -> 'a;
    exp : exp -> 'a;
    lval : lval -> 'a;
    varinfo : varinfo -> 'a;
    offset : offset -> 'a;
  }

  type 'a observer = {
    observe_exp : visitor:'a visitor -> exp -> 'a;
    observe_lval : visitor:'a visitor -> lval -> 'a;
    observe_varinfo : visitor:'a visitor -> varinfo -> 'a;
    observe_offset : visitor:'a visitor -> offset -> 'a;
  }

  val default : 'a observer
  val visit_exp : neutral:'a -> combine:('a -> 'a -> 'a) ->
    'a observer -> exp -> 'a
  val visit_lval : neutral:'a -> combine:('a -> 'a -> 'a) ->
    'a observer -> lval -> 'a
end
