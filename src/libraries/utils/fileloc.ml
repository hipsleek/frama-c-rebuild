(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* --- Datatype definition --- *)

module Prototype = struct

  type t = Filepos.t * Filepos.t [@@deriving eq, ord, show]

  let unknown = Filepos.(unknown, unknown)
  let reprs = [ unknown ]
  let copy = Datatype.identity
  let is_known loc = fst loc |> Filepos.is_known
  let hash loc = fst loc |> Filepos.hash
  let pretty_debug = pp

  type formats = {
    path: (string -> unit) Pretty.format;
    line: (int -> unit) Pretty.format;
    line_range: (int -> int -> unit) Pretty.format;
    column: (int -> unit) Pretty.format;
    column_range: (int -> int -> unit) Pretty.format;
    pos_range:
      (Filepos.t Pretty.aformatter -> Filepos.t ->
       Filepos.t Pretty.aformatter -> Filepos.t ->
       unit)  Pretty.format;
  }

  let pretty_generic ~formats fmt (pos_start, pos_end) =
    if not (Filepos.is_known pos_start) && not (Filepos.is_known pos_end) then
      Filepos.pretty fmt pos_start
    else
      let pos_start = Filepos.original pos_start
      and pos_end = Filepos.original pos_end in
      let path1 = Filepos.path pos_start and path2 = Filepos.path pos_end
      and line1 = Filepos.line pos_start and line2 = Filepos.line pos_end
      and col1 = Filepos.column pos_start and col2 = Filepos.column pos_end in
      if Filepath.equal path1 path2 then begin
        Format.fprintf fmt formats.path (Filepath.to_string path1);
        if line1 > 0 && line2 > 0 then
          if line1 <> line2 then
            Format.fprintf fmt formats.line_range line1 line2
          else begin
            Format.fprintf fmt formats.line line1;
            if col1 > 0 && col2 > 0 then begin
              if col1 <> col2 then
                Format.fprintf fmt formats.column_range col1 col2
              else
                Format.fprintf fmt formats.column col1
            end
          end
      end else
        Format.fprintf fmt formats.pos_range
          Filepos.pretty pos_start
          Filepos.pretty pos_end

  let pretty =
    let formats =
      { path = "%s";
        line = ":%d";
        line_range = ":%d-%d";
        column = ":%d";
        column_range = ":%d-%d";
        pos_range = "%a-%a"
      }
    in
    pretty_generic ~formats

  let pretty_long =
    let formats =
      { path = "%S";
        line = ", line %d";
        line_range = ", lines %d-%d";
        column = ", character %d";
        column_range = ", characters %d-%d";
        pos_range = "between %a and %a"
      }
    in
    pretty_generic ~formats

  let pretty_long_with_inclusions fmt loc =
    pretty_long fmt loc;
    List.pretty ~format:"%t" ~item:",@ included from %a" ~sep:"" ~empty:""
      Filepos.pretty fmt (Filepos.inclusions (fst loc))

end


include Datatype.Make_with_collections (struct
    include Datatype.Serializable_undefined
    include Prototype
    let name = "Fileloc"
  end)

include Prototype


(* --- Conversion from/to Lexing.position  --- *)

let of_lexing_loc (pos1, pos2) =
  Filepos.of_lexing_pos pos1, Filepos.of_lexing_pos pos2

let to_lexing_loc (pos1, pos2) =
  Filepos.to_lexing_pos pos1, Filepos.to_lexing_pos pos2


(* --- Accessors  --- *)

let path loc = fst loc |> Filepos.path

let line loc = fst loc |> Filepos.line


(* --- Datatype with comparison/hash on original source positions  --- *)

module Original = Datatype.Make_with_collections (struct
    include Datatype.Serializable_undefined
    include Prototype
    let name = "Fileloc.Original"
    type t = Filepos.Original.t * Filepos.Original.t [@@deriving eq, ord]
    let hash loc = fst loc |> Filepos.Original.hash
  end)
