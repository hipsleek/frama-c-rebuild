(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type location = Fileloc.t

(** Internal module holding the exception of [Error].

    The module is included into [Make_with_opt] later and should not be used
    directly. However we need to have a separate module for the exception so
    that every exception built by [Make] is the exact same type. *)
module Exn = struct
  exception Typing_error of location * Options.category option * string
  exception Not_yet of location * Options.category option * string
  exception Not_memoized of location * Options.category option
end

module type S = sig
  type 'a result = ('a, exn) Result.t
  include module type of Exn
  val make_untypable: ?loc:Fileloc.t -> string -> exn
  val make_not_yet: ?loc:Fileloc.t -> string -> exn
  val make_not_memoized: ?loc:Fileloc.t -> unit -> exn
  val untypable: ?loc:Fileloc.t -> string -> 'a
  val not_yet: ?loc:Fileloc.t -> string -> 'a
  val not_memoized: ?loc:Fileloc.t -> unit -> 'a
  val print_not_yet: string -> unit
  val handle: ('a -> 'a) -> 'a -> 'a
  val generic_handle: ('a -> 'b) -> 'b -> 'a -> 'b
  val retrieve_preprocessing:
    string ->
    ('a -> 'b result) ->
    'a ->
    (Format.formatter -> 'a -> unit) ->
    'b
  val pp_result:
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a result ->
    unit
  val iter : ('a -> unit) -> 'a result -> unit
  val map: ('a -> 'b) -> 'a result -> 'b
  val map2: ('a -> 'b -> 'c) -> 'a result -> 'b result -> 'c
  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a result -> 'b result -> 'c result -> 'd
end

module Make_with_opt(P: sig val phase:Options.category option end): S = struct
  type 'a result = ('a, exn) Result.t
  include Exn

  let make_untypable ?(loc = Current_loc.get ()) msg =
    Typing_error (loc, P.phase, msg)
  let make_not_yet ?(loc = Current_loc.get ()) msg =
    Not_yet (loc, P.phase, msg)
  let make_not_memoized ?(loc = Current_loc.get ()) () =
    Not_memoized (loc, P.phase)

  let untypable ?loc msg = raise (make_untypable ?loc msg)
  let not_yet ?loc msg = raise (make_not_yet ?loc msg)
  let not_memoized ?loc () = raise (make_not_memoized ?loc ())

  let pp_phase fmt phase =
    match phase with
    | Some phase ->
      if Options.verbose_atleast 2 then
        Format.fprintf fmt "@[@ in phase `%a'@]" Options.pp_category phase
    | None -> ()

  let do_print_not_yet phase msg =
    let msg =
      Format.asprintf
        "@[E-ACSL construct@ `%s'@ is not yet supported%a.@]"
        msg
        pp_phase phase
    in
    Options.warning
      ~once:true ~current:true
      "@[%s@ Ignoring annotation.@]" msg

  let print_not_yet msg =
    do_print_not_yet P.phase msg

  let with_loc loc f = Current_loc.with_loc loc f ()

  let generic_handle f res x =
    try
      f x
    with
    | Typing_error (loc, phase, s) ->
      with_loc loc @@ fun () ->
      let msg =
        Format.asprintf "@[invalid E-ACSL construct@ `%s'%a.@]"
          s
          pp_phase phase
      in
      Options.warning
        ~once:true ~current:true
        "@[%s@ Ignoring annotation.@]" msg;
      res
    | Not_yet (loc, phase, s) ->
      with_loc loc @@ fun () ->
      do_print_not_yet phase s;
      res

  let handle f x = generic_handle f x x

  let retrieve_preprocessing analyse_name getter parameter pp =
    try
      match getter parameter with
      | Result.Ok res -> res
      | Result.Error exn -> raise exn
    with Not_memoized (loc, phase) ->
      with_loc loc @@ fun () ->
      Options.fatal
        "@[%s was not performed on construct %a%a@]"
        analyse_name
        pp parameter
        pp_phase phase

  let pp_result pp fmt res =
    match res with
    | Result.Ok a -> Format.fprintf fmt "@[%a@]" pp a
    | Result.Error err -> Format.fprintf fmt "@[%s@]" (Printexc.to_string err)

  let iter f = function
    | Result.Ok a -> f a
    | Result.Error _ -> ()

  let map f = function
    | Result.Ok a -> f a
    | Result.Error e -> raise e

  let map2 f a b =
    match a,b with
    | Result.Ok a, Result.Ok b -> f a b
    | Result.Ok _, Result.Error e
    | Result.Error e, _ -> raise e

  let map3 f a b c =
    match a,b,c with
    | Result.Ok a, Result.Ok b, Result.Ok c -> f a b c
    | Result.Ok _, Result.Ok _, Result.Error e
    | Result.Ok _, Result.Error e, _
    | Result.Error e, _, _ -> raise e

end

module Make(P: sig val phase:Options.category end): S =
  Make_with_opt(struct let phase = Some P.phase end)

include Make_with_opt(struct let phase = None end)
