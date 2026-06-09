(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** A reference to the current location. If you are careful to set this to
    the current location then you can use some built-in logging functions that
    will print the location.
*)
include State_builder.Ref with type data = Filepos.t * Filepos.t

(** [with_loc loc f x] set the current location to [loc], which can be used
    with [Current_loc.get ()] or via the option [~current] in Log functions.
    The old location is saved and set back after execution of [f x]. If [f x]
    raises an exception, it is caught and re-raised after setting the location
    to its old value.
*)
val with_loc : data -> ('a -> 'b) -> 'a -> 'b

(** Same behavior than [with_loc] but takes an location option. The location is
    set to [loc] for [Some loc] and unchanged otherwise.
*)
val with_loc_opt : data option -> ('a -> 'b) -> 'a -> 'b

module Operators : sig

  (** [UpdatedCurrentLoc] is a simple constructor which can be used as
      documentation : [let<> UpdatedCurrentLoc = loc in ...] or replaced with
      [_]
  *)
  type operation = UpdatedCurrentLoc

  (** [let<> UpdatedCurrentLoc = loc in ...] can be used to mimic the behavior
      obtained with [with_loc loc f UpdatedCurrentLoc].
      [let<>] syntax requires to open the module [Operators].
  *)
  val ( let<> ) : data -> (operation -> 'a) -> 'a

  (** Same behavior than [let<>] but for [with_loc_opt] *)
  val ( let<?> ) : data option -> (operation -> 'a) -> 'a
end
