(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

type input =
  | StdlibIn of in_channel
  | CompressedIn of Compression.in_channel

type output =
  | StdlibOut of out_channel
  | CompressedOut of Compression.out_channel

let is_compressed_chan chan =
  (* Requires that chan is a channel opened at the beginning of a file. *)
  let res =
    try
      let char1 = Stdlib.input_byte chan in
      let char2 = Stdlib.input_byte chan in
      (* zip magic number: 1f 8b *)
      char1 = 0x1f && char2 = 0x8b
    with _ -> (* error reading magic number, assume uncompressed *)
      false
  in
  Stdlib.seek_in chan 0;
  res

let open_in_bin file =
  let chan = Stdlib.open_in_bin file in
  if is_compressed_chan chan then
    try
      CompressedIn (Compression.open_in_chan chan)
    with exn ->
      Stdlib.close_in_noerr chan; raise exn
  else
    StdlibIn chan

let close_in = function
  | StdlibIn chan -> Stdlib.close_in chan
  | CompressedIn chan -> Compression.close_in chan

let input_value = function
  | StdlibIn chan ->
    Stdlib.input_value chan
  | CompressedIn chan ->
    Compression.input_value chan

let input_char = function
  | StdlibIn chan ->
    Stdlib.input_char chan
  | CompressedIn chan ->
    Compression.input_char chan

let unsafe_really_input = function
  | StdlibIn chan ->
    Stdlib.unsafe_really_input chan
  | CompressedIn chan ->
    Compression.unsafe_really_input chan

let open_out_bin ?(compress=false) file =
  let chan = Stdlib.open_out_bin file in
  if compress then
    try
      CompressedOut (Compression.open_out_chan chan)
    with exn ->
      Stdlib.close_out_noerr chan; raise exn
  else
    StdlibOut chan

let close_out = function
  | StdlibOut chan -> Stdlib.close_out chan
  | CompressedOut chan -> Compression.close_out chan

let output_value = function
  | StdlibOut chan ->
    Stdlib.output_value chan
  | CompressedOut chan ->
    Compression.output_value chan
