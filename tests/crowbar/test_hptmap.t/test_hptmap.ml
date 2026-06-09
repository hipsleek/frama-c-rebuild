module Int = (struct
  include Datatype.Int
  let id x = x
end)

module Map = Map.Make (struct
    type t = int
    let compare = compare
  end)

module Info = struct
  let initial_values = []
  let dependencies = [ Ast.self ]
end

module IMap = Hptmap.Make (Int) (Int) (Info)

type t = (int Map.t * IMap.t)

let check_hptmap ((map, hptmap) : t) =
  Map.for_all (fun k v -> IMap.find k hptmap = v) map &&
  IMap.for_all (fun k v -> Map.find k map = v) hptmap

let apply decide (l, m) (l', m') =
  Map.union (fun k a b -> Some (decide k a b)) l l',
  IMap.join ~cache:NoCache ~symmetric:false ~idempotent:true ~decide m m'

let hptmap =
  let open Crowbar in
  fix
    (fun hptmap ->
       choose [
         const (Map.empty, IMap.empty);
         map [uint8; uint8; hptmap] (fun k v (l, m) -> Map.add k v l, IMap.add k v m);
         map [uint8; uint8] (fun k v -> Map.singleton k v, IMap.singleton k v);
         map [uint8; hptmap] (fun k (l, m) -> Map.remove k l, IMap.remove k m);
         map [hptmap; hptmap] (apply (fun _k a _b -> a));
       ])

let f () =
  Crowbar.add_test ~name:"hptmap" [hptmap]
    (fun m -> Crowbar.check (check_hptmap m))

let () = Crowbar_utils.run "hptmap" f
