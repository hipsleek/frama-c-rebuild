(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Mt_types

type access_or_protection = Unaccessed | Mutexes of MutexPresence.t

module UnaccessedOrProtection = struct

  let pretty fmt = function
    | Unaccessed -> Format.fprintf fmt "unaccessed"
    | Mutexes p ->
      if MutexPresence.is_empty p
      then Format.fprintf fmt "unprotected"
      else Format.fprintf fmt "protected by %a" MutexPresence.pretty p

  let equal v1 v2 = match v1, v2 with
    | Unaccessed, Unaccessed -> true
    | Mutexes p1, Mutexes p2 -> MutexPresence.equal p1 p2
    | _ -> false

  let compare v1 v2 = match v1, v2 with
    | Unaccessed, Unaccessed -> 0
    | Mutexes p1, Mutexes p2 -> MutexPresence.compare p1 p2
    | Unaccessed, Mutexes _ -> -1
    | Mutexes _, Unaccessed -> 1


  let hash = function
    | Unaccessed -> 3
    | Mutexes p -> 3 + MutexPresence.hash p

  let join v1 v2 = match v1, v2 with
    | Unaccessed, Unaccessed -> Unaccessed
    | Unaccessed, Mutexes p | Mutexes p, Unaccessed -> Mutexes p
    | Mutexes p1, Mutexes p2 -> Mutexes (MutexPresence.combine p1 p2)

  let meet _ _ = assert false

end

type mutexes_by_access = {
  mutexes_for_read: access_or_protection;
  mutexes_for_write: access_or_protection;
}

module MutexesByAccess = struct

  type t = mutexes_by_access

  let same_read_write { mutexes_for_read = r; mutexes_for_write = w } =
    UnaccessedOrProtection.equal r  w

  let pretty fmt ({ mutexes_for_read = r; mutexes_for_write = w } as v) =
    if same_read_write v then
      UnaccessedOrProtection.pretty fmt r
    else
      Format.fprintf fmt "write %a,@ read %a"
        UnaccessedOrProtection.pretty w
        UnaccessedOrProtection.pretty r

  let hash { mutexes_for_read = r; mutexes_for_write = w } =
    UnaccessedOrProtection.hash r + UnaccessedOrProtection.hash w


  let equal v1 v2 =
    UnaccessedOrProtection.equal v1.mutexes_for_read v2.mutexes_for_read &&
    UnaccessedOrProtection.equal v1.mutexes_for_write v2.mutexes_for_write

  let compare v1 v2 =
    let (<?>) c lcmp = if c <> 0 then c else Lazy.force lcmp in
    UnaccessedOrProtection.compare v1.mutexes_for_read v2.mutexes_for_read <?>
    lazy (UnaccessedOrProtection.compare v1.mutexes_for_write v2.mutexes_for_write)

  let join v1 v2 = {
    mutexes_for_read = UnaccessedOrProtection.join
        v1.mutexes_for_read v2.mutexes_for_read;
    mutexes_for_write = UnaccessedOrProtection.join
        v1.mutexes_for_write v2.mutexes_for_write;
  }

  let meet v1 v2 = {
    mutexes_for_read = UnaccessedOrProtection.meet
        v1.mutexes_for_read v2.mutexes_for_read;
    mutexes_for_write = UnaccessedOrProtection.meet
        v1.mutexes_for_write v2.mutexes_for_write;
  }

  let unaccessed = { mutexes_for_read = Unaccessed;
                     mutexes_for_write = Unaccessed }
end


module LatticeMutexes = struct
  include Datatype.Make(
    struct
      include Datatype.Undefined
      include MutexesByAccess

      let structural_descr = Structural_descr.t_abstract

      let reprs = [{ mutexes_for_read = Mutexes MutexPresence.empty;
                     mutexes_for_write = Mutexes MutexPresence.empty }]
      let name = "Mt_shared_vars_types.LatticeMutexes.t"


      let rehash x = x
    end)

  let bottom = { mutexes_for_read = Unaccessed; mutexes_for_write = Unaccessed }
  let top = { mutexes_for_read = Mutexes MutexPresence.empty;
              mutexes_for_write = Mutexes MutexPresence.empty }

  let join = MutexesByAccess.join
  let _meet = MutexesByAccess.meet

  (* ZZZ improve complexity *)
  let is_included v1 v2 = (join v1 v2) = v2

  let default = bottom
  let default_is_bottom = true
end


module MutexesByZone = struct
  include Lmap_bitwise.Make_bitwise(LatticeMutexes)

  let pretty =
    pretty_generic_printer ~pretty_v:LatticeMutexes.pretty
      ~skip_v:(fun v -> LatticeMutexes.(equal v bottom))
      ~sep:""
      ()
end
