(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** The set of functions in Filesystem are provided both as a convenient way
    to use {!Filepath.t} directly (without conversion) and to be safer variants
    than the standard library's or Unix library's.

    They are safer in several ways:

    - some of them are intended to never fail ({!dir_exists}, {!remove_file},
      etc.);
    - some of them return a [Result.t] ({!file_kind}, {!with_open_in}, etc.)
      which forces the caller to be careful about the possible errors;
    - the others will uniformly raise {!Sys_error} - possibly converted from
      {!Unix.Unix_error} - so the handling of exceptions is a bit lighter;
    - all the functions taking a file path as argument will check that the
      path is not empty and may raise {!Invalid_argument} if it is not the case.

    The module documentation should mention all the possible exceptions raised
    and the caller should always catch {!Sys_error} if needed. Empty file paths
    are considered a programming error, and the emptiness should be checked
    by the caller beforehand. Thus, the caller should not catch
    {!Invalid_argument}.
*)

(* ************************************************************************* *)
(** {2 Error handling} *)
(* ************************************************************************* *)

type error = string * Filepath.t
type nonrec 'a result = ('a,error) result


(* ************************************************************************* *)
(** {2 File system} *)
(* ************************************************************************* *)

(** This type is used to determine the type of file a path refers to.
    @since 32.0-Germanium *)
type file_kind =
  | File
  | Directory
  | CharacterDevice
  | BlockDevice
  | SymbolicLink
  | NamedPipe
  | Socket

(** [file_kind p] returns the file kind of the given path [p]. On failure -
    for instance if the file does not exist - returns an error string.
    @raise Invalid_argument if [p] is empty
    @since 32.0-Germanium *)
val file_kind: Filepath.t -> file_kind result

(** [exists p] returns whether the path [p] points to an existing file (of any
    kind) [p]. Equivalent to {!Sys.file_exists}.
    @raise Invalid_argument if the path is empty
    @since 28.0-Nickel *)
val exists: Filepath.t -> bool

(** [file_exists p] returns whether the path points to an existing regular file.
    It is equivalent to [file_kind p = Ok (File)]
    @raise Invalid_argument if [p] is empty
    @since 32.0-Germanium *)
val file_exists: Filepath.t -> bool

(** [dir_exists p] returns whether the path points to an existing directory,
    It is equivalent to [file_kind p = Ok (Directory)]
    @raise Invalid_argument if [p] is empty
    @since 32.0-Germanium *)
val dir_exists: Filepath.t -> bool

(** Contents of a directory.
    @raise Sys_error if a system error occurred
    @raise Invalid_argument if the path is empty
    @since 31.0-Gallium *)
val list_dir: Filepath.t -> string list

(** Iter through the contents of a directory.
    @raise Sys_error if a system error occurred
    @raise Invalid_argument if the path is empty
    @since 31.0-Gallium *)
val iter_dir: (string -> unit) -> Filepath.t -> unit

(** Fold over the contents of a directory.
    @raise Sys_error if a system error occurred
    @raise Invalid_argument if the path is empty
    @since 31.0-Gallium *)
val fold_dir: (string -> 'a -> 'a) -> Filepath.t -> 'a -> 'a

(** [make_dir ?parents ?perm filepath] creates directory [filepath] with
    permission [perm] (default is 0o755). If the directory already exists, this
    function does nothing. However, if the path points to an existing file that
    is not a directory, the function raises [Sys_error]. If [parents] is true
    (the default), recursively create parent directories if needed.
    Note that this function may create some of the parent directories
    and then fail to create the children, e.g. if [perm] does not allow
    user execution of the created directory. This will leave the filesystem
    in a modified state before raising {!Sys_error}.
    @raise Sys_error if a system error occurred
    @raise Invalid_argument if the path is empty
    @since 19.0-Potassium
    @before 28.0-Nickel [name] argument was of type [string]. Also, the function
    did not check for path's existence.
    @before 32.0-Germanium the function raised {!Invalid_argument} instead of
    {!Sys_error} when the path pointed to an existing file that was not a
    directory. Also the [perm] argument was not named and the return type was
    [bool] to indicate whether the directory has actually been created or if it
    already existed. *)
val make_dir : ?parents:bool -> ?perm:int -> Filepath.t -> unit

(** Tries to delete a file and never fails.
    @before 31.0-Gallium it was Extlib.safe_remove *)
val remove_file: Filepath.t -> unit

(** Tries to delete a directory and never fails.
    @before 31.0-Gallium it was Extlib.safe_remove_dir *)
val remove_dir: Filepath.t -> unit

(** [rename source target] rename the file [source] to [target]. Equivalent to
    {!Sys.rename}.
    @raise Sys_error if a system error occurred
    @raise Invalid_argument if one of the paths is empty
    @since 28.0-Nickel *)
val rename: Filepath.t -> Filepath.t -> unit


(* ************************************************************************* *)
(** {2 Temporary files} *)
(* ************************************************************************* *)

(** See {!Temp_files} module for automatic removal of temp files at exit. *)

(** Similar to {!Filename.temp_file}.
    @raise Sys_error if the temp file cannot be created.
    @since 31.0-Gallium
    @before 32.0-Germanium raised a removed [Temp_file] exception *)
val temp_file: prefix:string -> suffix:string -> Filepath.t

(** Similar to {!Filename.temp_dir}.
    @raise Sys_error if the temp dir cannot be created.
    @since 31.0-Gallium
    @before 32.0-Germanium raised a removed [Temp_file] exception *)
val temp_dir: prefix:string -> suffix:string -> Filepath.t


(* ************************************************************************* *)
(** {2 File comparison} *)
(* ************************************************************************* *)

(** [digest p] computes the hash of a file [p] using {!Stdlib.Digest.file}.
    @raise Sys_error if a system error occurred
    @raise Invalid_argument if the path is empty
    @since 31.0-Gallium *)
val digest: Filepath.t -> string

(** [same_digest p1 p2] compares the hashes of two files [p1] and [p2] using
    {!Stdlib.Digest.file} and returns [true] if they have the same.
    @raise Sys_error if a system error occurred
    @raise Invalid_argument if the path is empty
    @since 31.0-Gallium *)
val same_digest: Filepath.t -> Filepath.t -> bool


(* ************************************************************************* *)
(** {2 High level Input/Output} *)
(* ************************************************************************* *)

(** [copy_file source target] copies source file to target file.
    @raise Sys_error if a system error occurred
    @raise Invalid_argument if one of the paths is empty
    @since 31.0-Gallium
    @before 31.0-Gallium this function was {!Command.copy} *)
val copy_file : Filepath.t -> Filepath.t -> unit

(** Iter over all text lines in the file
    @raise Sys_error if a system error occurred
    @raise Invalid_argument if the path is empty
    @since 31.0-Gallium
    @before 31.0-Gallium this function was {!Command.read_lines} *)
val iter_lines : Filepath.t -> (string -> unit) -> unit

(** Iter over all text lines and line number in the file
    @raise Sys_error if a system error occurred
    @raise Invalid_argument if the path is empty
    @since Frama-C+dev *)
val iteri_lines : Filepath.t -> (int -> string -> unit) -> unit

(** [iter_line_range p i j job] iter over the lines [i] to [j] (included) from
    file [p]. [job] is called for every matching line if it exists.
    @raise Sys_error if a system error occurred
    @raise Invalid_argument if the path is empty
    @since Frama-C+dev *)
val iter_line_range :
  Filepath.t -> int -> int -> (int -> string -> unit) -> unit

(* ************************************************************************* *)
(** {2 Low level file Input/Output} *)
(* ************************************************************************* *)

(** This type defines what action {!with_open_in} and {!with_open_out} must
    perform when the file to open does not exist. *)
type action_if_missing =
  | Create of int (** create the file with the given permissions *)
  | DoNotCreate (** do not create the file and fail *)

(** This type define what action {!with_open_out} must perform when the file to
    open already exists. *)
type action_if_exists =
  | Error (** file opening functions will fail with an error *)
  | Append (** the writing contents will be appended *)
  | Truncate (** the file will be truncated before any writes *)

(** A [safe_processor] helps to handle file operations while ensuring the
    file will be closed no matter what happens. It is a function that takes
    a file operation [f] as a parameter, opens a file and calls the [f] with
    the newly-created channel. *)
type ('ch,'a) safe_processor = ('ch -> 'a) -> 'a result

(** Same as {!safe_processor} but when a {!Sys_error} is raised, re-raise it
    after closing the file *)
type ('ch,'a) exn_processor = ('ch -> 'a) -> 'a

(** [with_open_in path f] opens file [path] for reading and calls [f] with the
    newly-created input channel. The file is closed when [f] returns or whenever
    a {!Sys_error} is thrown by [f].
    @param if_missing defines what must be done if the file does not exist,
    defaults to [DoNotCreate].
    @param binary must be set if the file needs to be opened in binary mode
    (disables conversion, e.g. new lines), defaults to [false]
    @param blocking must be unset if the file needs to be opened in nonblocking
    mode, defaults to [true].
    @raise Invalid_argument if the path is empty
    @return [Ok (f input_channel)] if no {!Sys_error}s are thrown, or [Error s]
    if a [Sys_error s] is thrown during the execution of [f] or during the
    closing of the file.
    @since 31.0-Gallium *)
val with_open_in:
  ?if_missing:action_if_missing ->
  ?binary:bool ->
  ?blocking:bool ->
  Filepath.t ->
  (in_channel, 'a) safe_processor

(** Same as {!with_open_in} but raises {!Sys_error} instead of returning [Error].
    @since 31.0-Gallium
    @raise Sys_error if a system error occurred
    @raise Invalid_argument if the path is empty
    @before 31.0-Gallium this function was [Command.read_file] *)
val with_open_in_exn :
  ?if_missing:action_if_missing ->
  ?binary:bool ->
  ?blocking:bool ->
  Filepath.t ->
  (in_channel, 'a) exn_processor

(** [with_open_out path f] calls [f] with a new output channel on the file [path]
    opened for writing. The file is closed when [f] returns or whenever a
    {!Sys_error} is thrown by [f].
    @param if_missing defines what must be done if the file does not exist,
    defaults to [Create 0o666].
    @param if_exists defines what action must be performed when the file already
    exists, defaults to {!Truncate}.
    @param binary must be set if the file needs to be opened in binary mode
    (disables conversion, e.g. new lines), defaults to [false].
    @param blocking must be unset if the file needs to be opened in nonblocking
    mode, defaults to [true].
    @raise Invalid_argument if the path is empty
    @return [Ok (f output_channel)] if no {!Sys_error}s are thrown, or [Error s]
    if a [Sys_error s] is thrown during the execution of [f] or during the
    closing the file.
    @since 31.0-Gallium *)
val with_open_out:
  ?if_missing:action_if_missing ->
  ?if_exists:action_if_exists ->
  ?binary:bool ->
  ?blocking:bool ->
  Filepath.t ->
  (out_channel, 'a) safe_processor

(** Same as {!with_open_out} but raises {!Sys_error} instead of returning
    [Error].
    @since 31.0-Gallium
    @raise Sys_error if a system error occurred
    @raise Invalid_argument if the path is empty
    @before 31.0-Gallium this function was [Command.write_file] *)
val with_open_out_exn:
  ?if_missing:action_if_missing ->
  ?if_exists:action_if_exists ->
  ?binary:bool ->
  ?blocking:bool ->
  Filepath.t ->
  (out_channel, 'a) exn_processor


(** [with_formatter path f] calls [f] with a formatter writing to the file
    [path]. The file is closed and the formatter is flushed when [f] returns or
    whenever a {!Sys_error} is thrown by [f].
    @raise Invalid_argument if the path is empty
    @return [Ok (f fmt)] if no {!Sys_error}s are thrown, or [Error s]
    if a [Sys_error s] is thrown during the execution of [f] or when
    closing the file.
    @since 31.0-Gallium *)
val with_formatter: Filepath.t -> (Format.formatter, 'a) safe_processor

(** Same as {!with_formatter} but raises {!Sys_error} instead of returning
    [Error].
    @raise Sys_error if a system error occurred
    @raise Invalid_argument if the path is empty
    @since 31.0-Gallium
    @before 31.0-Gallium this function was [Command.pp_to_file] and
    [Command.print_file] *)
val with_formatter_exn: Filepath.t -> (Format.formatter, 'a) exn_processor

module Compressed : sig
  (** [with_open_in_exn path f] calls [f] with a new input channel on the file
      [path] opened for reading in binary mode. If the file is compressed, then
      the input channel is uncompressed. The file is closed when [f] returns or
      whenever a {!Sys_error} is thrown by [f].

      Note: this function should be merged with existing [with_open_in...]
      functions at some point.
      @raise Sys_error if a system error occurred
      @raise Invalid_argument if the path is empty
  *)
  val with_open_in_exn :
    Filepath.t ->
    (Channel.input, 'a) exn_processor

  (** [with_open_out_bin_exn ?compress path f] calls [f] with a new output
      channel on the file [path] opened for writing in binary mode. If
      [compress] is [true] then then content of the file will be compressed by
      [Compression]. The file is closed when [f] returns or whenever a
      {!Sys_error} is thrown by [f].

      Note: this function should be merged with existing [with_open_out...]
      functions at some point.
      @raise Sys_error if a system error occurred
      @raise Invalid_argument if the path is empty *)
  val with_open_out_exn :
    ?compress:bool ->
    Filepath.t ->
    (Channel.output, 'a) exn_processor
end

(** Opening this module allows to use shorter syntax to deal with files.

    {[
      let open Filesystem.Operators in
      let result =
        let+ channel = Filesystem.with_open_out filepath in
        output_string channel "42";
      in
      match result with
      | Ok () -> ()
      | Error error ->
        Format.printf "error writing to file %a: %s"
          Filepath.pretty filepath
          error
    ]}

    When the file processing returns a result by itself, the operator [let*]
    can be used instead:

    {[
      let open Filesystem.Operators in
      let* channel = Filesystem.with_open_in filepath in
      try
        let header = input_line channel in
        if header = "42"
        then Ok ()
        else Error "wrong file header"
      with End_of_file ->
        Error "file is empty"
    ]} *)
module Operators : sig
  (** {3 Result operators}
      These operators are intended to be used with {!with_open_in} or {!with_open_out}.
  *)

  val (let+): ('ch,'a) safe_processor -> ('ch -> 'a) -> 'a result
  val (let*):
    ('ch,'a result) safe_processor ->
    ('ch -> 'a result) ->
    'a result

  (** {3 Sys_error operators}
      These operators are intended to be used with {!with_open_in_exn} or
      {!with_open_out_exn}, exception {!Sys_error} must be caught.
  *)

  val (let$): ('ch,'a) exn_processor -> ('ch -> 'a) -> 'a
end
