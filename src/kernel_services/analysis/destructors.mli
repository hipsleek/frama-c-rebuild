(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** retrieve local variables with [__fc_destructor] attribute and add
    the appropriate calls to the corresponding destructor function when we
    exit the scope of the variable.

    Argument of the attribute can take the following forms:
    - [AStr f], where [f] is the name of the function to call.
    - [ACons (f, [AInt n])], where [f] is the name of the function to call and
      n an argument that will be passed to f in addition to the variable.
      Will be used for destructing local C++ arrays.
    - [AAddrOf a] where [a] is of the form above, to indicate that the
      destructor should be given the address of the variable and not the
      variable directly
*)

(** category of the transformation. Should be done after any transformation
    susceptible to change the CFG of the program (e.g.
    {!Exn_flow.transform_category})
*)
val transform_category: File.code_transformation_category
