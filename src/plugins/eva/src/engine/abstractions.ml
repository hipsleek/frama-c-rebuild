(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)



(* --- Contexts abstraction ------------------------------------------------- *)

module Context = struct
  type 'c structure = 'c Abstract.Context.structure
  type 'c dependencies = 'c Abstract_context.dependencies
  let dec_eq = Abstract.Context.eq_structure

  type 'c context = (module Abstract_context.Leaf with type t = 'c)

  (* When building the abstraction, we will need to compare the dependencies
     structure with the structured values. *)
  let rec outline : type v. v dependencies -> v structure = function
    | Leaf context ->
      let module C = (val context) in
      Abstract.Context.Leaf (C.key, (module C))
    | Node (l, r) -> Abstract.Context.(Node (outline l, outline r))

  (* Folding over contexts dependencies *)
  type 'a folder = { folder : 'c. 'c context -> 'a -> 'a }
  let rec fold : type c. 'a folder -> c dependencies -> 'a -> 'a =
    fun folder dependencies acc ->
    match dependencies with
    | Leaf leaf -> folder.folder leaf acc
    | Node (l, r) -> fold folder l (fold folder r acc)

  (* The context abstraction build consists of accumulating registered contexts
     into a structured context and then adding the needed operators, thus making
     it interactive. A [Unit] structured abstraction is used for the initial
     state and is discarded as soon as a real context abstraction is added. *)
  module type Structured = Abstract.Context.Internal
  module type Interactive = Abstract.Context.External
  type 'a or_unit = Unit | Context of 'a
  type structured = (module Structured) or_unit
  type interactive = (module Interactive) or_unit

  let init : structured = Unit

  (* During the complete abstraction build, we need to verify that there is at
     least one context in the computed abstraction.
     TODO: better error handling. *)
  let assert_not_unit = function
    | Unit -> Self.fatal "The built context cannot be unit."
    | Context interactive -> interactive

  (* Making a structured context interactive simply consists of adding the
     needed operations using the Structure.Open functor.*)
  let make_interactive : structured -> interactive = function
    | Unit -> Unit
    | Context (module Structured) ->
      Context (module struct
        include Structured
        include Structure.Open (Abstract.Context) (Structured)
      end)

  (* Adding a registered context into a structured one consists of deciding if
     a product is needed (which comes down to checking if the registered
     context key we want to add is not in the structure), computing it, and
     updating the structure. *)
  let add : type c. c context -> structured -> structured =
    fun (module Ctx) structured ->
    let leaf = Abstract.Context.Leaf (Ctx.key, (module Ctx)) in
    match make_interactive structured with
    | Unit ->
      Context (module struct
        include Ctx
        let structure = leaf
      end)
    | Context (module Interactive) when not (Interactive.mem Ctx.key) ->
      Context (module struct
        include Context_product.Make (Interactive) (Ctx)
        let structure = Abstract.Context.Node (Interactive.structure, leaf)
      end)
    | _ -> structured


  (* When building the complete abstraction, we need to trick values and domains
     into thinking that their context dependencies are there, even if the
     structured context type is not the good one. This is done through a
     lift that requires conversion operations to interact with the subpart
     of the structured context that matters for the value or the domain.
     This functor is responsible of building such conversion operations. *)
  module type From = sig type context val structure : context structure end
  module Converter (From : From) (To : Interactive) = struct
    type internal = From.context
    type extended = To.t
    let structure = From.structure

    let void_context () =
      Self.fatal "Cannot register a context module from a Void structure."

    let rec set : type v. v structure -> v -> extended -> extended = function
      | Leaf (key, _) -> To.set key
      | Node (s1, s2) -> fun (v1, v2) ext -> set s2 v2 ext |> set s1 v1
      | Option (s, default) -> fun v -> set s (Option.value ~default v)
      | Unit -> fun () value -> value
      | Void _ -> void_context ()

    let rec get : type v. v structure -> extended -> v = function
      | Leaf (key, _) -> Option.get (To.get key)
      | Node (s1, s2) -> fun v -> get s1 v, get s2 v
      | Option (s, _) -> fun v -> Some (get s v)
      | Unit -> fun _ -> ()
      | Void _ -> void_context ()

    let replace = set structure
    let extend v = replace v To.top
    let restrict = get structure
  end
end



(* --- Values abstraction --------------------------------------------------- *)

module Value = struct
  type 'v structure = 'v Abstract.Value.structure
  type 'v key = 'v Abstract.Value.key
  type 'v dependencies = 'v Abstract_value.dependencies
  let dec_eq = Abstract.Value.eq_structure

  type 'v value = (module Abstract_value.Leaf with type t = 'v)

  (* When building the abstraction, we will need to compare the dependencies
     structure with the structured values. *)
  let rec outline : type v. v dependencies -> v structure = function
    | Leaf value ->
      let module V = (val value) in
      Abstract.Value.Leaf (V.key, (module V))
    | Node (l, r) -> Abstract.Value.(Node (outline l, outline r))

  (* Folding over values dependencies *)
  type 'a folder = { folder : 'v. 'v value -> 'a -> 'a }
  let rec fold : type v. 'a folder -> v dependencies -> 'a -> 'a =
    fun folder dependencies acc ->
    match dependencies with
    | Leaf leaf -> folder.folder leaf acc
    | Node (l, r) -> fold folder l (fold folder r acc)

  (* Folding over contexts dependencies of a value. *)
  let rec fold_contexts : type v. 'a Context.folder -> v dependencies -> 'a -> 'a =
    fun folder dependencies acc ->
    match dependencies with
    | Leaf (module V) -> Context.fold folder V.context acc
    | Node (l, r) -> fold_contexts folder l (fold_contexts folder r acc)


  (* As for the context abstraction, building the value abstraction consists
     of structuring the needed registered values and then adding the needed
     operators to make it interactive. However, a structured value is not
     as simple as a structured context, as it needs to keep track of the
     context abstraction it is based on. This context is supposed to be the
     complete aggregation of all the contexts that are needed by the
     requested domains. *)
  module type Structured = sig
    type context
    module Context : Context.Interactive with type t = context
    module Value : Abstract.Value.Internal with type context = context
  end

  (* We expose the type of the structured context we are based on to statically
     ensure that we do not temper with it. As for the context abstractions, a
     [Unit] structured abstraction is used for the initial state and is
     discarded as soon as a value is added. *)
  type ('c, 'v) or_unit = Unit of 'c | Value of 'v
  type 'c context = (module Context.Interactive with type t = 'c)
  type 'c structured_module = (module Structured with type context = 'c)
  type 'c structured = ('c context, 'c structured_module) or_unit

  module type Interactive = Abstract.Value.External
  type 'c interactive_module = (module Interactive with type context = 'c)
  type 'c interactive = ('c context, 'c interactive_module) or_unit


  (* Initial value builder *)
  let init (context : 'c context) : 'c structured = Unit context

  (* During the complete abstraction build, we need to verify that there is at
     least one value in the computed abstraction.
     TODO: better error handling. *)
  let assert_not_unit = function
    | Unit _ -> Self.fatal "The built value cannot be unit."
    | Value interactive -> interactive


  (* Making a structured value interactive simply consists of adding the
     needed operations using the Structure.Open functor. *)
  let make_interactive : type c. c structured -> c interactive = function
    | Unit context -> Unit context
    | Value (module Structured) ->
      Value (module struct
        include Structured.Value
        include Structure.Open (Abstract.Value) (Structured.Value)
      end)

  (* Retrieves the context contained in a structured value. *)
  let get_context : type c. c structured -> c context = function
    | Unit context -> context
    | Value (module V) -> (module V.Context)


  (* Adding a registered value into a structured one consists of deciding if
     a product is needed (which comes down to checking if the registered
     value key we want to add is not in the structure), computing it, and
     updating the structure. *)
  let add : type c v. v value -> c structured -> c structured =
    fun (module Leaf) structured ->
    let leaf_context_structure = Context.outline Leaf.context in
    let module To = (val get_context structured) in
    let lifted_leaf : (module Abstract.Value.Internal with type context = c) =
      match Context.dec_eq leaf_context_structure To.structure with
      | Some Eq ->
        let leaf = Abstract.Value.Leaf (Leaf.key, (module Leaf)) in
        (module struct include Leaf let structure = leaf end)
      | None ->
        let module From = struct
          type context = Leaf.context
          let structure = leaf_context_structure
        end in
        let module Converter = Context.Converter (From) (To) in
        (module Value_lift.Make (Leaf) (Converter))
    in
    let combined : (module Abstract.Value.Internal with type context = c) =
      match make_interactive structured with
      | Unit _ -> lifted_leaf
      | Value (module Val) when Val.mem Leaf.key -> (module Val)
      | Value (module Val) ->
        (module Value_product.Make (To) (val lifted_leaf) (Val))
    in
    Value (module struct
      type context = c
      module Context = To
      module Value = (val combined)
    end)


  (* When building the complete abstraction, we need to trick locations and
     domains into thinking that their value dependencies are there, even if
     the structured value type is not the good one. This is done through a
     lift that requires conversion operations to interact with the subpart
     of the structured value that matters for the location or the domain.
     This functor is responsible of building such conversion operations. *)
  module type From = sig type value val structure : value structure end
  module Converter (From : From) (To : Interactive) = struct
    type internal = From.value
    type extended = To.t
    let structure = From.structure

    let void_value () =
      Self.fatal "Cannot register a value module from a Void structure."

    let rec set : type v. v structure -> v -> extended -> extended = function
      | Leaf (key, _) -> To.set key
      | Node (s1, s2) -> fun (v1, v2) ext -> set s2 v2 ext |> set s1 v1
      | Option (s, default) -> fun v -> set s (Option.value ~default v)
      | Unit -> fun () value -> value
      | Void _ -> void_value ()

    let rec get : type v. v structure -> extended -> v = function
      | Leaf (key, _) -> Option.get (To.get key)
      | Node (s1, s2) -> fun v -> get s1 v, get s2 v
      | Option (s, _) -> fun v -> Some (get s v)
      | Unit -> fun _ -> ()
      | Void _ -> void_value ()

    let replace = set structure
    let extend v = replace v To.top
    let restrict = get structure
  end
end



(* --- Locations abstraction ------------------------------------------------ *)

module Location = struct
  type 'l structure = 'l Abstract.Location.structure
  type 'l dependencies = 'l Abstract_location.dependencies
  let dec_eq = Abstract.Location.eq_structure

  type 'l location = (module Abstract_location.Leaf with type location = 'l)

  (* When building the abstraction, we will need to compare the dependencies
     structure with the structured values. *)
  let rec outline: type v. v dependencies -> v structure = function
    | Leaf location ->
      let module Loc = (val location) in
      Abstract.Location.Leaf (Loc.key, (module Loc))
    | Node (l, r) -> Abstract.Location.(Node (outline l, outline r))

  (* Folding over values dependencies *)
  type 'a folder = { folder : 'l. 'l location -> 'a -> 'a }
  let rec fold : type l. 'a folder -> l dependencies -> 'a -> 'a =
    fun folder dependencies acc ->
    match dependencies with
    | Leaf leaf -> folder.folder leaf acc
    | Node (l, r) -> fold folder l (fold folder r acc)

  (* Folding over the values dependencies of some locations dependencies. *)
  let rec fold_values : type l. 'a Value.folder -> l dependencies -> 'a -> 'a =
    fun folder dependencies acc ->
    match dependencies with
    | Leaf (module Loc) -> Value.fold folder Loc.value acc
    | Node (l, r) -> fold_values folder l (fold_values folder r acc)

  (* Folding over contexts dependencies of the value dependencies of some
     locations dependencies. *)
  let rec fold_contexts : type l. 'a Context.folder -> l dependencies -> 'a -> 'a =
    fun folder dependencies acc ->
    match dependencies with
    | Leaf (module Loc) -> Value.fold_contexts folder Loc.value acc
    | Node (l, r) -> fold_contexts folder l (fold_contexts folder r acc)


  (* As for the value abstraction, building the location abstraction consists
     of structuring the needed registered locations and then adding the needed
     operators to make it interactive. However, a structured location is not
     as simple as a structured value, as it needs to keep track of the value
     abstraction it is based on. This value is supposed to be the complete
     aggregation of all the values that are needed by the requested domains. *)
  module type Structured = sig
    type value
    module Value : Value.Interactive with type t = value
    module Location : Abstract.Location.Internal with type value = value
  end

  module type Interactive = Abstract.Location.External

  (* We expose the type of the structured value we are based on to statically
     ensure that we do not temper with it. As for the value abstractions, a
     [Unit] structured abstraction is used for the initial state and is
     discarded as soon as a location is added. *)
  type ('u, 'l) or_unit = Unit of 'u | Location of 'l
  type 'v value = (module Value.Interactive with type t = 'v)
  type 'v structured_module = (module Structured with type value = 'v)
  type 'v structured = ('v value, 'v structured_module) or_unit
  type 'v interactive_module = (module Interactive with type value = 'v)
  type 'v interactive = ('v value, 'v interactive_module) or_unit

  (* Initial location builder *)
  let init (value : 'v value) : 'v structured = Unit value

  (* During the complete abstraction build, we need to verify that there is at
     least one location in the computed abstraction.
     TODO: better error handling. *)
  let assert_not_unit = function
    | Unit _ -> Self.fatal "The built location cannot be unit."
    | Location interactive -> interactive


  (* Making a structured value interactive simply consists of adding the
     needed operations using the Structure.Open functor.*)
  let make_interactive : type v. v structured -> v interactive = function
    | Unit value -> Unit value
    | Location (module Structured) ->
      Location (module struct
        include Structured.Location
        include Structure.Open (Abstract.Location) (struct
            include Structured.Location
            type t = location
          end)
      end)

  (* Retrieves the value contained in a structured location. *)
  let get_value : type v. v structured -> v value = function
    | Unit value -> value
    | Location (module S) -> (module S.Value)


  (* Adding a registered location into a structured one is done in three steps:
     1. Lifting the location abstraction we want to add to match the value
        abstraction contained in the structured abstraction.
     2. Combine the given location abstraction with the one contained in the
        structured abstraction. It comes down to decide if a reduced product is
        needed.
     3. Rebuild a structured abstraction with the new location abstraction. *)
  let add : type v l. l location -> v structured -> v structured =
    fun (module Leaf) structured ->
    let leaf_value_structure = Value.outline Leaf.value in
    let module To = (val get_value structured) in
    let lifted_leaf : (module Abstract.Location.Internal with type value = v) =
      match Value.dec_eq leaf_value_structure To.structure with
      | Some Eq ->
        let leaf = Abstract.Location.Leaf (Leaf.key, (module Leaf)) in
        (module struct include Leaf let structure = leaf end)
      | None ->
        let module From = struct
          type value = Leaf.value
          let structure = leaf_value_structure
        end in
        let module Converter = Value.Converter (From) (To) in
        (module Location_lift.Make (Leaf) (Converter))
    in
    let combined : (module Abstract.Location.Internal with type value = v) =
      match make_interactive structured with
      | Unit _ -> lifted_leaf
      | Location (module Loc) when Loc.mem Leaf.key -> (module Loc)
      | Location (module Loc) ->
        (module Locations_product.Make (To) (val lifted_leaf) (Loc))
    in
    Location (module struct
      type value = v
      module Value = To
      module Location = (val combined)
    end)


  (* When building the complete abstraction, we need to trick domains into
     thinking that their locations dependencies are there, even if the
     structured location type is not the good one. This is done through a
     lift that requires conversion operations to interact with the subpart
     of the structured location that matters for the domains. This functor is
     responsible of building such conversion operations. *)
  module type From = sig type location val structure : location structure end
  module Converter (From : From) (To : Interactive) = struct
    type internal = From.location
    type extended = To.location
    let structure = From.structure

    let void_location () =
      Self.fatal "Cannot register a location module from a Void structure."

    let rec set : type l. l structure -> l -> extended -> extended = function
      | Leaf (key, _) -> To.set key
      | Node (s1, s2) -> fun (l1, l2) ext -> set s2 l2 ext |> set s1 l1
      | Option (s, default) -> fun l -> set s (Option.value ~default l)
      | Unit -> fun () loc -> loc
      | Void _ -> void_location ()

    let rec get : type l. l structure -> extended -> l = function
      | Leaf (key, _) -> Option.get (To.get key)
      | Node (s1, s2) -> fun l -> get s1 l, get s2 l
      | Option (s, _) -> fun l -> Some (get s l)
      | Unit -> fun _ -> ()
      | Void _ -> void_location ()

    let replace = set structure
    let extend l = replace l To.top
    let restrict = get structure
  end
end



(* --- Domains abstraction -------------------------------------------------- *)

module Domain = struct
  module type S = Abstract_domain.S
  module type Context = Abstract.Context.External
  module type Value = Abstract.Value.External

  (** Functor domain which can be built over any value abstractions, but with
      fixed locations dependencies. *)
  module type Functor = sig
    type location
    val location_dependencies: location Abstract_location.dependencies
    module Make (C : Context) (V : Value with type context = C.t) : sig
      include Abstract_domain.S
        with type context = C.t
         and type value = V.t
         and type location = location
      val key : state Abstract_domain.key
    end
  end

  (* To simplify the domain registration procedure, we provide common types.
     However, the code above is still useful to prove some properties, mainly
     that we do not temper with the dependencies. *)
  type domain =
    | Domain : (module Abstract_domain.Leaf) -> domain
    | Functor : (module Functor) -> domain

  (* Information about a domain: name, experimental status and priority rank. *)
  type domain_info =
    { name : string
    ; experimental : bool
    ; priority : int
    }

  (* A registered domain: the domain information, a function that builds
     the domain, and a condition which automatically enables the domain. *)
  type registered =
    { info : domain_info
    ; domain : unit -> domain
    ; auto_enable : unit -> bool
    }

  (* The configuration of an analysis contains a set of registered domains
     along with their analysis mode. *)
  type registered_with_mode = registered * Domain_mode.t option

  (* Mutable list containing statically and dynamically registered domains. *)
  let domains_list = ref []

  let register_domain
      ?(experimental=false) ?(priority=0) ?(auto_enable=fun () -> false)
      ~name ~descr domain =
    let descr = if experimental then "Experimental. " ^ descr else descr in
    Parameters.register_domain ~name ~descr ~priority;
    let info = { name ; experimental ; priority } in
    let registered = { info ; domain ; auto_enable } in
    domains_list := registered :: !domains_list;
    registered

  (* Registration of a leaf domain. *)
  let register ~name ~descr ?experimental ?priority ?auto_enable domain =
    register_domain ~name ~descr ?experimental ?priority ?auto_enable
      (fun () -> Domain domain)

  (* Registration of a functor domain. *)
  let register_functor ~name ~descr ?experimental ?priority ?auto_enable domain =
    register_domain ~name ~descr ?experimental ?priority ?auto_enable
      (fun () -> Functor domain)

  (* Registration of a dynamic domain. *)
  let dynamic_register ~name ~descr ?experimental ?priority ?auto_enable make =
    register_domain ~name ~descr ?experimental ?priority ?auto_enable
      (fun () -> Domain (make ()))


  (* Building the domain abstraction consists of structuring the requested
     registered domains. To do so, we need to keep track of the values and
     locations abstraction on which the structured domain will rely. Those
     abstractions are supposed to be the complete aggregations of all the
     values (resp locations) that are needed by the requested domains. *)
  module type Structured = sig
    type context
    type value
    type location
    module Context : Context.Interactive
      with type t = context
    module Value : Value.Interactive
      with type context = context
       and type t = value
    module Location : Location.Interactive
      with type value = value
       and type location = location
    module Domain : Abstract.Domain.Internal
      with type context = context
       and type value = value
       and type location = location
  end

  type 'c context =
    (module Context.Interactive with type t = 'c)

  type ('c, 'v) value =
    (module Value.Interactive with type context = 'c and type t = 'v)

  type ('v, 'l) location =
    (module Location.Interactive with type value = 'v and type location = 'l)

  type ('c, 'v, 'l) state =
    (module Structured
      with type context = 'c
       and type value = 'v
       and type location = 'l)

  type ('c, 'v, 'l, 's) or_unit = Unit of 'c * 'v * 'l | State of 's

  type ('c, 'v, 'l) structured =
    ('c context, ('c, 'v) value, ('v, 'l) location, ('c, 'v, 'l) state) or_unit

  (* Internal type used for intermediate results of the add procedure. *)
  type ('c, 'v, 'l) structured_domain =
    (module Abstract.Domain.Internal
      with type context = 'c
       and type value = 'v
       and type location = 'l)


  let init c v l : ('c, 'v, 'l) structured = Unit (c, v, l)

  (* During the complete abstraction build, we need to verify that there is at
     least one domain in the computed abstraction.
     TODO: better error handling. *)
  let assert_not_unit = function
    | Unit _ -> Self.fatal "The built domain cannot be unit."
    | State structured -> structured

  module type Typ = sig type t [@@warning "-34"] end
  type 't typ = (module Typ with type t = 't)

  type ('internal, 'extended) conversion =
    (module Domain_lift.Conversion
      with type extended = 'extended
       and type internal = 'internal)

  let conversion_id (type t) (_ : t typ) : (t, t) conversion =
    (module struct
      type extended = t
      type internal = t
      let extend x = x
      let restrict x = x
    end)

  let context : type c v l. (c, v, l) structured -> c context = function
    | Unit (context, _, _) -> context
    | State (module S) -> (module S.Context)

  let value : type c v l. (c, v, l) structured -> (c, v) value = function
    | Unit (_, value, _) -> value
    | State (module S) -> (module S.Value)

  let location : type c v l. (c, v, l) structured -> (v, l) location = function
    | Unit (_, _, location) -> location
    | State (module S) -> (module S.Location)

  type 'a identity = 'a -> 'a

  (* Adding a registered domain into a structured one consists of performing a
     lifting of the registered one if needed before performing the product,
     configuring the name and restricting the domain depending of the mode. *)
  type input = domain_info * domain * Domain_mode.t option
  let add : type c v l. input -> (c, v, l) structured identity =
    fun (info, domain, mode) structured ->
    let wkey = Self.wkey_experimental in
    if info.experimental
    then Self.warning ~wkey "The %s domain is experimental." info.name;
    let module Ctx = (val context structured) in
    let module Val = (val value structured) in
    let module Loc = (val location structured) in
    let lifted : (c, v, l) structured_domain =
      match domain with
      | Functor (module Functor) ->
        let locs = Location.outline Functor.location_dependencies in
        let eq_loc = Location.dec_eq locs Loc.structure in
        let module D = Functor.Make (Ctx) (Val) in
        begin match eq_loc with
          | Some Eq ->
            let structure = Abstract.Domain.Leaf (D.key, (module D)) in
            (module struct include D let structure = structure end)
          | None ->
            let module Ctx = (val conversion_id (module Ctx)) in
            let module Val = (val conversion_id (module Val)) in
            let module From = struct include D let structure = locs end in
            let module Loc = Location.Converter (From) (Loc) in
            (module Domain_lift.Make (D) (Ctx) (Val) (Loc))
        end
      | Domain (module D) ->
        let ctx_deps = Context.outline D.context_dependencies in
        let val_deps = Value.outline D.value_dependencies in
        let loc_deps = Location.outline D.location_dependencies in
        let eq_ctx = Context.dec_eq ctx_deps Ctx.structure in
        let eq_val = Value.dec_eq val_deps Val.structure in
        let eq_loc = Location.dec_eq loc_deps Loc.structure in
        match eq_ctx, eq_val, eq_loc with
        | Some Eq, Some Eq, Some Eq ->
          let structure = Abstract.Domain.Leaf (D.key, (module D)) in
          (module struct include D let structure = structure end)
        | _ ->
          let ctx_converter : (D.context, Ctx.t) conversion =
            match eq_ctx with
            | Some Eq -> conversion_id (module Ctx)
            | None ->
              let module From = struct include D let structure = ctx_deps end in
              (module Context.Converter (From) (Ctx))
          in
          let val_converter : (D.value, Val.t) conversion =
            match eq_val with
            | Some Eq -> conversion_id (module Val)
            | None ->
              let module From = struct include D let structure = val_deps end in
              (module Value.Converter (From) (Val))
          in
          let loc_converter : (D.location, Loc.location) conversion =
            match eq_loc with
            | Some Eq -> conversion_id (module (struct type t = Loc.location end))
            | None ->
              let module From = struct include D let structure = loc_deps end in
              (module Location.Converter (From) (Loc))
          in
          (module Domain_lift.Make (D)
               (val ctx_converter) (val val_converter) (val loc_converter))
    in
    (* Restricts the domain according to [mode]. *)
    let restricted : (c, v, l) structured_domain =
      match mode with
      | None -> lifted
      | Some kf_modes ->
        let module Scope = struct let functions = kf_modes end in
        (module Domain_builder.Restrict (Ctx) (Val) (val lifted) (Scope))
    in
    let combined : (c, v, l) structured_domain =
      match structured with
      | Unit _ -> restricted
      | State (module Structured) ->
        (* The new [domain] becomes the left leaf of the domain product, and
           will be processed before the domains from [Acc.Dom] during the
           analysis. *)
        let module Dom = Structured.Domain in
        (module Domain_product.Make (Ctx) (Val) (Loc) (val restricted) (Dom))
    in
    State (module struct
      type context = c
      type value = v
      type location = l
      module Context = Ctx
      module Value = Val
      module Location = Loc
      module Domain = (val combined)
    end)



  let add_contexts contexts (_info, domain, _mode) =
    let add_context = Context.{ folder = add } in
    match domain with
    | Domain (module Domain) ->
      Context.fold add_context Domain.context_dependencies contexts |>
      Value.fold_contexts add_context Domain.value_dependencies |>
      Location.fold_contexts add_context Domain.location_dependencies
    | Functor (module F) ->
      Location.fold_contexts add_context F.location_dependencies contexts

  let add_values values (_info, domain, _mode) =
    let add_value = Value.{ folder = add } in
    match domain with
    | Domain (module Domain) ->
      Value.fold add_value Domain.value_dependencies values |>
      Location.fold_values add_value Domain.location_dependencies
    | Functor (module F) ->
      Location.fold_values add_value F.location_dependencies values

  let add_locations locs (_info, domain, _mode) =
    let add = Location.{ folder = add } in
    match domain with
    | Domain  (module D) -> Location.fold add D.location_dependencies locs
    | Functor (module D) -> Location.fold add D.location_dependencies locs

  let build domains =
    let make (registered, mode) = registered.info, registered.domain (), mode in
    let domains = List.map make domains in
    (* Build the contexts *)
    let contexts = List.fold_left add_contexts Context.init domains in
    let interactive_ctx c = Context.(make_interactive c |> assert_not_unit) in
    let module Contexts = (val interactive_ctx contexts) in
    (* Build the values *)
    let init_values = Value.init (module Contexts) in
    let values = List.fold_left add_values init_values domains in
    let interactive_value v = Value.(make_interactive v |> assert_not_unit) in
    let module Values = (val interactive_value values) in
    (* Build the locations *)
    let init_locations = Location.init (module Values) in
    let locations = List.fold_left add_locations init_locations domains in
    let interactive_loc l = Location.(make_interactive l |> assert_not_unit) in
    let module Locs = (val interactive_loc locations) in
    (* Build the domains *)
    let init_domain = init (module Contexts) (module Values) (module Locs) in
    let structured = List.fold_left (fun s d -> add d s) init_domain domains in
    let module Structured = (val assert_not_unit structured) in
    (module Structured : Structured)
end



(* --- Configuration -------------------------------------------------------- *)

module Config = struct
  module Mode = Datatype.Option (Domain_mode)

  include Set.Make (struct
      open Domain
      type t = registered_with_mode
      let compare (d1, m1) (d2, m2) =
        let c = Datatype.Int.compare d1.info.priority d2.info.priority in
        if c = 0 then
          let c = Datatype.String.compare d1.info.name d2.info.name in
          if c = 0 then Mode.compare m1 m2 else c
        else c
    end)

  let configure () =
    let find = Parameters.DomainsFunction.find in
    let find name = try Some (find name) with Not_found -> None in
    let main () = Globals.entry_point () |> fst in
    let add_main_mode modes = (main (), Domain_mode.Mode.all) :: modes in
    let add config (registered : Domain.registered) =
      let name = registered.info.name in
      if not (Parameters.Domains.mem name) && registered.auto_enable () then
        (* Add the auto-enabled domain to the options so that it appears
           enabled in the GUI. *)
        Parameters.Domains.add name;
      let enabled = Parameters.Domains.mem name in
      let enable modes = if enabled then add_main_mode modes else modes in
      match find name with
      | None -> if enabled then add (registered, None) config else config
      | Some modes -> add (registered, Some (enable modes)) config
    in
    List.fold_left add empty !Domain.domains_list
end



(* --- Value reduced product ----------------------------------------------- *)

module type Value_with_reduction = sig
  include Abstract.Value.External
  val reduce : t -> t
end

module Reducer = struct
  type 'a key = 'a Value.key
  type ('a, 'b) reducer = 'a -> 'b -> 'a * 'b
  type action = Action : 'a key * 'b key * ('a, 'b) reducer -> action

  let actions = ref []

  let register left right reducer =
    actions := (Action (left, right, reducer)) :: !actions

  module Make (Value : Abstract.Value.External) = struct
    include Value

    let make_reduction acc (Action (key1, key2, f)) =
      match Value.get key1, Value.get key2 with
      | Some get1, Some get2 ->
        let set1 = Value.set key1 and set2 = Value.set key2 in
        let reduce v = f (get1 v) (get2 v) in
        let reducer v = let v1, v2 = reduce v in set1 v1 (set2 v2 v) in
        reducer :: acc
      | _, _ -> acc

    let reduce =
      let list = List.fold_left make_reduction [] !actions in
      fun v -> List.fold_left (fun v reduce -> reduce v) v list
  end
end



(* --- Finalizing abstractions build ---------------------------------------- *)

module type S = sig
  module Ctx : Abstract.Context.External
  module Val : Value_with_reduction with type context = Ctx.t
  module Loc : Abstract.Location.External with type value = Val.t
  module Dom : Abstract.Domain.External
    with type value = Val.t
     and type location = Loc.location
     and type context = Ctx.t
end

module Hooks = struct
  let hooks = ref []
  type hook = (module S) -> (module S)
  let register (f : hook) = hooks := f :: !hooks
  let apply abst = List.fold_left (fun acc f -> f acc) abst !hooks
end

module Open (Structured : Domain.Structured) : S = struct
  module Ctx = Structured.Context
  module Val = Reducer.Make (Structured.Value)
  module Loc = Structured.Location
  module Dom = struct
    include Structured.Domain
    include Structure.Open (Abstract.Domain) (Structured.Domain)
  end
end

let make config =
  let abstractions = Config.elements config |> Domain.build in
  let abstractions = (module Open (val abstractions) : S) in
  Hooks.apply abstractions
