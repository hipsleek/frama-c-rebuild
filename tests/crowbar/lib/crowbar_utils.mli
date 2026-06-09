val generate_cil_file : string -> Cil_types.file

val generate_file : Cil_types.file -> unit

val run : string -> (unit -> 'a) -> 'a
