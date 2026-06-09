(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** [validate s] returns
    - [None] when [s] is UTF-8 compliant
    - [Some (line,byte_pos_line,byte_pos_error)] otherwise.
      note: the first error is at the just after the byte located at
            position [byte_pos_error] (that is also just after the byte
            [byte_pos_error-byte_pos_line] of the line number [line]).
*)
val validate: string -> (int * int * int) option
