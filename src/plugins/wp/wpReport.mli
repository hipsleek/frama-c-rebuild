(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Export Statistics.

    Patterns for formatting:
    - ["%{cmd:arg}"] or "%cmd:arg"
    - ["%{cmd}"] or ["%cmd"]

    Patterns in [fct]:
    - ["%kf"] or ["%kf:name"] the name of the function.
    - ["%kf:<s>"] the stats in format [<s>] for the function.
    - ["%<p>:<s>"] the stats in format [<s>] for prover [<p>].

    Patterns in [main]:
    - "%<s>" the global statistics with format [<s>].

    Prover strings are ["wp"], ["ergo"], ["coq"] , ["z3"] and ["simplify"].
    Format strings are "100" (percents of valid upon total, default),
      ["total"], ["valid"] and ["failed"]
      for respective number of verification conditions.
    Zero is printed as [zero]. Percentages are printed in decimal ["dd.d"].

*)

type fcstat

val fcstat : unit -> fcstat

val export : fcstat -> Filepath.t -> unit
val export_json : fcstat -> ?jinput:string -> joutput:string -> unit -> unit
