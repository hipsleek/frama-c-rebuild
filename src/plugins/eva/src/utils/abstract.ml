(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

module Context = struct

  type 'a context = (module Abstract_context.S with type t = 'a)
  module C = struct type 'a t = 'a context end
  include Structure.Shape (Structure.Key_Context) (C)

  module type Internal = sig
    include Abstract_context.S
    val structure : t structure
  end

  module type External = sig
    include Internal
    include Structure.External
      with type t := t
       and type 'a key := 'a key
       and type 'a data := 'a data
  end

end


module Value = struct

  module V = struct
    type 'a t = (module Abstract_value.S with type t = 'a)
  end

  include Structure.Shape (Structure.Key_Value) (V)

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

module Location = struct

  module L = struct
    type 'a t = (module Abstract_location.S with type location = 'a)
  end

  include Structure.Shape (Structure.Key_Location) (L)

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

module Domain = struct

  module D = struct
    type 'a t = (module Abstract_domain.S with type state = 'a)
  end

  include Structure.Shape (Structure.Key_Domain) (D)

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
