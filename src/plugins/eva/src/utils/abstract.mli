(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Internal and External signature of abstractions used in the Eva engine. *)

(** Internal modules contains a [structure] value that describes the internal
    structure of the abstraction. This structure is used to automatically
    generate efficient accessors from a generic compound abstraction to specific
    leaf abstractions. *)

(** External modules export direct accessors to their leaf components.
    When a generic abstraction is a product of several specific abstractions,
    they allow interacting with each leaf abstraction identified by a key.
    Note that their behavior is undefined if an abstraction contains
    several times the same leaf module. *)

(** Key and structure for abstract contexts.
    See {!Structure} for more details. *)
module Context : sig
  include Structure.Shape
    with type 'a key = 'a Structure.Key_Context.key
     and type 'a data = (module Abstract_context.S with type t = 'a)

  module type Internal = sig
    include Abstract_context.S
    val structure: t structure
  end

  module type External = sig
    include Internal
    include Structure.External with type t := t
                                and type 'a key := 'a key
                                and type 'a data := 'a data
  end
end

(** Key and structure for abstract values.
    See {!Structure} for more details. *)
module Value : sig
  include Structure.Shape
    with type 'a key = 'a Structure.Key_Value.key
     and type 'a data = (module Abstract_value.S with type t = 'a)

  module type Internal = sig
    include Abstract_value.S
    val structure: t structure
  end

  module type External = sig
    include Internal
    include Structure.External with type t := t
                                and type 'a key := 'a key
                                and type 'a data := 'a data
  end
end

(** Key and structure for abstract locations.
    See {!Structure} for more details. *)
module Location : sig
  include Structure.Shape
    with type 'a key = 'a Structure.Key_Location.key
     and type 'a data = (module Abstract_location.S with type location = 'a)

  module type Internal = sig
    include Abstract_location.S
    val structure: location structure
  end

  module type External = sig
    include Internal
    include Structure.External with type t := location
                                and type 'a key := 'a key
                                and type 'a data := 'a data
  end
end

(** Key and structure for abstract domains.
    See {!Structure} for more details. *)
module Domain : sig
  include Structure.Shape
    with type 'a key = 'a Structure.Key_Domain.key
     and type 'a data = (module Abstract_domain.S with type state = 'a)

  module type Internal = sig
    include Abstract_domain.S
    val structure: t structure
  end

  module type External = sig
    include Internal
    include Structure.External with type t := t
                                and type 'a key := 'a key
                                and type 'a data := 'a data
  end
end
