(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** A builtin takes the state and a list of values for the arguments, and
    returns the return value (which can be bottom), and a boolean indicating the
    possibility of alarms.  *)
type str_builtin_sig =
  Cvalue.Model.t -> Cvalue.V.t list -> Cvalue.V.t * bool

val frama_c_strlen_wrapper: str_builtin_sig
val frama_c_wcslen_wrapper: str_builtin_sig
val frama_c_strchr_wrapper: str_builtin_sig
val frama_c_wcschr_wrapper: str_builtin_sig
val frama_c_memchr_off_wrapper: str_builtin_sig
val frama_c_wmemchr_off_wrapper: str_builtin_sig
