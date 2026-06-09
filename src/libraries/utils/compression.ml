(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** File compression *)

let input_value chan =
  let bufsize = Marshal.header_size + (4 * 1024) in
  let buffer = Bytes.create bufsize in
  Gzip.really_input chan buffer 0 Marshal.header_size;
  let data_size = Marshal.data_size buffer 0 in
  let buffer =
    if data_size > bufsize then
      Bytes.extend buffer 0 (bufsize - data_size)
    else
      buffer
  in
  Gzip.really_input chan buffer Marshal.header_size data_size;
  Marshal.from_bytes buffer 0

let unsafe_really_input chan buf ofs len =
  (* Gzip.unsafe_really_input does not exist but Unmarshal uses this function to
     copy bytes into an array "created" with Obj.obj, i.e. with its length
     incorrectly reported. Since the length of the array is incorrectly reported
     we cannot juste use really_input instead. This function creates first a
     bytes array of the correct length to be able to use really_input, then copy
     the content to buf with unsafe_blit so that the length of buf is not
     checked. *)
  let bytes = Bytes.create len in
  Gzip.really_input chan bytes 0 len;
  Bytes.unsafe_blit bytes 0 buf ofs len

let output_value chan value =
  let bytes = Marshal.to_bytes value [] in
  Gzip.output chan bytes 0 (Bytes.length bytes)

include Gzip
