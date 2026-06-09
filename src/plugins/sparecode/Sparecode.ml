(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(*
  Internal documentation.

  The Sparecode module aims at removing the unused code.

  It is composed of to parts :
  - one (in module {!module:Marks}) that computes some information
    to say what has to be kept in the result. It uses the generic PDG
    marking facility {{:../pdg/PdgMarks.ml}PdgMarks} and
    {{:../pdg/Marks.ml}Marks},
  - and a second one (module {!module:Transform}) that read those results to
    produce a new application. This part mainly use the kernel AST
    transformation Filter which provides a functor that filters an application
    to create another one.

  To select the useful statements, we start from the [main] outputs and the
  reachable annotations, and mark backward all the dependencies. When reaching
  a function call, the called function statements are also marked according to
  the needed outputs, but the inputs are not propagated immediately because it
  would make every function call visible. The information provided by the PDG
  marking system is kept to be used later.

  So, after the first step, we iterate on the input marks to propagate, and
  propagate them only for the visible calls, ie those which have at least one
  visible output. This process is repeated as long as there are some
  modification.
*)

module Register = Register
