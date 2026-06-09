(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(** Creation of the initial state for Value *)

open Cil_types

let add_initialized state loc v =
  Cvalue.Model.add_binding ~exact:true state loc v

let make_well hidden_base state loc =
  let size = Bit_utils.max_bit_size () in
  let well =
    Cvalue.V.inject_top_origin Origin.well (Base.Hptset.singleton hidden_base)
  in
  let well_loc =
    Locations.make
      (Addresses.Bits.inject hidden_base Ival.zero)
      (`Value size)
  in
  let state = add_initialized state well_loc well in
  add_initialized state loc well


let warn_unknown_size_aux pp v (messt, t) =
  Self.warning ~once:true ~current:true
    ~wkey:Self.wkey_unknown_size
    "@[during initialization@ of %a,@ size of@ type '%a'@ cannot be@ computed@ \
     (%s)@]" pp v Printer.pp_typ t messt

let warn_unknown_size =
  warn_unknown_size_aux
    (fun fmt v -> Format.fprintf fmt "variable '%a'" Printer.pp_varinfo v)

type validity_hidden_base =
  | UnknownValidity (* Base is maybe invalid on its entire validity *)
  | KnownThenUnknownValidity of Z.t (* Base is valid on i bits, then
                                             maybe invalid on the remainder of its validity *)

let stdlib_attribute = (Ast_attributes.fc_stdlib_generated, [])

let create_hidden_base ~libc ~valid ~hidden_var_name ~name_desc pointed_typ =
  let hidden_var = Eva_utils.create_new_var hidden_var_name pointed_typ in
  if libc
  then hidden_var.vattr <- Ast_attributes.add stdlib_attribute hidden_var.vattr;
  hidden_var.vdescr <- Some name_desc;
  let validity =
    (* Add a special case for void* pointers: we do not want to compute the
       size of void *)
    let validity = match Ast_types.unroll_node pointed_typ with
      | TVoid -> Base.Unknown (Z.zero, None, Bit_utils.max_bit_address ())
      | _ -> Base.validity_from_type hidden_var
    in
    match validity with
    | Base.Known (a,b)
      when not (Parameters.ContextValidPointers.get ()) ->
      (* Weaken validity, because the created variables are not supposed
         to be valid *)
      (match valid with
       | KnownThenUnknownValidity size -> (*except here, for size bits*)
         let size = Z.pred size in
         assert (Z.leq size b);
         Base.Unknown (a, Some size, b)
       | UnknownValidity -> Base.Unknown (a, None, b)
      )
    | Base.Unknown _ -> (* Unknown validity is caused by strange type *)
      Self.result ~dkey:Self.dkey_initial_state
        "creating variable %s with imprecise size (type %a)"
        hidden_var_name Printer.pp_typ pointed_typ;
      validity
    | Base.Empty | Base.Known _ | Base.Invalid -> validity
    | Base.Variable _ -> (* should never happen (validity_from_type cannot
                            return Weak) *) assert false
  in
  Base.register_memory_var hidden_var validity


let reject_empty_struct b offset typ =
  match Ast_types.unroll_node typ with
  | TComp ci ->
    if ci.cfields = Some [] && not (Machine.acceptEmptyCompinfo ()) then
      Self.abort ~current:true
        "@[empty %ss@ are unsupported@ (type '%a',@ location %a%a)@ \
         in C99 (only allowed on GCC/MSVC machdep).@ Aborting.@]"
        (if ci.cstruct then "struct" else "union")
        Printer.pp_typ typ Base.pretty b Printer.pp_offset offset
  | _ -> ()


(** [initialize_var_using_type varinfo state] uses the type of [varinfo]
    to create an initial value in [state]. *)
let initialize_var_using_type varinfo state =
  let rec add_offsetmap depth b name_desc name typ offset_orig typ_orig state =
    let typ = Ast_types.unroll typ in
    let loc = lazy (Locations.of_type_offset b typ_orig offset_orig) in
    let bind_entire_loc ?(state=state) v = (* Shortcut *)
      add_initialized state (Lazy.force loc) v
    in
    match typ.tnode with
    | TInt _ | TEnum _ ->
      bind_entire_loc Cvalue.V.top_int

    | TFloat fkind -> begin
        (* TODO: depend on the option for finitness *)
        bind_entire_loc
          (Cvalue.V.inject_float (Fval.top_finite (Fval.kind fkind)))
      end

    | TFun _ -> state

    | TPtr typ'
      when depth <= Parameters.ContextDepth.get () ->
      let attr = Ast_types.get_attributes typ in
      let libc = Cil.is_in_libc varinfo.vattr in
      let context_max_width =
        Parameters.ContextWidth.get ()
      in begin
        match Ast_types.is_void typ', Ast_types.is_fun typ' with
        | false, false -> (* non-void, non-function *)
          let i =
            match Ast_attributes.find_params "arraylen" attr with
            | [AInt i] -> i
            | _ -> Z.of_int context_max_width
          in
          let arr_pointed_typ =
            Cil_const.mk_tarray typ' (Some (Cil.kinteger64 ~loc:varinfo.vdecl i))
          in
          let hidden_var_name =
            Cabs2cil.fresh_global ("S_" ^ name)
          in
          let name_desc = "*"^name_desc in
          (* Make first cell of the array valid. The NULL pointer takes
             care of a potential invalid pointer. *)
          let valid =
            try KnownThenUnknownValidity (Z.of_int (Cil.bitsSizeOf typ'))
            with Cil.SizeOfError _ -> UnknownValidity
          in
          let hidden_base =
            create_hidden_base
              ~libc ~valid ~hidden_var_name ~name_desc arr_pointed_typ
          in
          let state =
            add_offsetmap
              (depth + 1)
              hidden_base
              name_desc
              hidden_var_name
              arr_pointed_typ
              NoOffset
              arr_pointed_typ
              state
          in
          let value = Cvalue.V.inject hidden_base (Ival.zero) in
          let value =
            if Parameters.ContextValidPointers.get ()
            then value
            else Cvalue.V.join Cvalue.V.singleton_zero value
          in
          bind_entire_loc ~state value
        | true, false -> (* void *)
          let hidden_var_name = Cabs2cil.fresh_global ("S_" ^ name) in
          let name_desc = "*"^name_desc in
          let valid = UnknownValidity in
          let hidden_base =
            create_hidden_base ~libc ~valid ~hidden_var_name ~name_desc typ'
          in
          make_well hidden_base state (Lazy.force loc)
        | false, true -> (* function *)
          (* Generating functions is next to useless for the user (what
             does the function do), and too dangerous for the AST. *)
          bind_entire_loc ~state Cvalue.V.singleton_zero
        | true, true -> assert false (* inconsistent *)
      end

    | TArray (typ, len) ->
      begin try
          let size = Cil.lenOfArray len in
          let size_elt = Z.of_int (Cil.bitsSizeOf typ) in
          let psize = pred size in
          let state = ref state in
          let typ = Ast_types.unroll typ in
          let max_precise_size =
            Parameters.ContextWidth.get ()
          in
          let locs = ref [] in
          for i = 0 to min psize (pred max_precise_size) do
            (* Cells that are treated really precisely. We create new
               pointers (if needed) for each distinct cell *)
            let offset =
              Cil.addOffset
                (Index (Cil.integer ~loc:varinfo.vdecl i, NoOffset))
                offset_orig
            in
            let name = string_of_int i ^ "_" ^ name in
            let name_desc = name_desc ^ "[" ^ string_of_int i ^ "]" in
            state :=
              add_offsetmap depth b name_desc name typ offset typ_orig !state;
            let loc = Locations.of_type_offset b typ_orig offset in
            locs := loc :: !locs;
          done;
          if max_precise_size < size then begin
            (* Some elements remain to be initialized *)
            let offsm_of_loc loc = (* This rereads one of the first cells*)
              let offsm =
                Cvalue.Model.copy_offsetmap loc size_elt !state
              in
              match offsm with `Bottom -> assert false | `Value m -> m
            in
            let last_loc, locs = match !locs with
              | [] -> assert false (* ContextWidth is at least 1*)
              | l :: ll -> l, ll
            in
            let last_offsm = offsm_of_loc last_loc.addr in
            (* Join of the contents of the first elements *)
            let aux_loc offsm (loc: Locations.t) =
              Cvalue.V_Offsetmap.join offsm (offsm_of_loc loc.addr)
            in
            let offsm_joined = List.fold_left aux_loc last_offsm locs in
            (* TODO: add Offsetmap.paste_repeated_slices to Offsetmap, and
               replace everything below by a call to it. *)
            let nb_fields =
              Cvalue.V_Offsetmap.fold (fun _itv _ -> succ) offsm_joined 0
            in
            if nb_fields = 1 then
              (* offsm_joined is very regular (typically Top_int, or some
                 pointers). We read its contents and copy it everywhere else.
                 The periodicity of the contents may be smaller than the size
                 of a cell; take this into account. *)
              let v, modu, offset =
                Option.get (Cvalue.V_Offsetmap.fold
                              (fun _itv v _ -> Some v) offsm_joined None)
              in
              assert (Abstract_interp.Rel.is_zero offset);
              let ncells = size - max_precise_size in
              let total_size = Z.mul size_elt (Z.of_int ncells) in
              let offsm_repeat = Cvalue.V_Offsetmap.create
                  ~size_v:modu ~size:total_size v in
              let addr = Addresses.Bits.shift
                  (Ival.inject_singleton size_elt) last_loc.addr;
              in
              (* paste [size - max_precise_size] elements, starting from
                 the last location initialized + 1 *)
              state :=
                Cvalue.Model.paste_offsetmap
                  ~from:offsm_repeat ~dst_addr:addr ~size:total_size ~exact:true
                  !state
            else (
              (* We have probably initialized a struct with different fields.
                 We must perform offsetmap copies, that are slower *)
              if nb_fields * psize >= 5000 then
                Self.result ~once:true ~current:true
                  "Initializing a complex array of %d elements. This may \
                   take some time" size;
              let addr = ref last_loc.addr in
              for _i = max_precise_size to psize do
                addr := Addresses.Bits.shift
                    (Ival.inject_singleton size_elt) !addr;
                state :=
                  Cvalue.Model.paste_offsetmap
                    ~from:offsm_joined ~dst_addr:!addr ~size:size_elt ~exact:true
                    !state
              done);
          end;
          !state
        with
        | Cil.LenOfArray cause ->
          Self.warning ~wkey:Self.wkey_unknown_size ~once:true ~current:true
            "problem with array size (%a) for variable %a, assuming 0"
            Cil.pp_incorrect_array_length cause Printer.pp_varinfo varinfo;
          (* This is either a flexible array member (for which Cil
             implicitly returns a size of 0, so we are doing the proper
             thing), or an incomplete array (which is forbidden)  *)
          state
        | Cil.SizeOfError (s, t) ->
          warn_unknown_size varinfo (s, t);
          bind_entire_loc Cvalue.V.top_int;
      end

    | TComp ({cstruct=true} as compinfo) -> (* Struct *)
      reject_empty_struct b offset_orig typ;
      let treat_field state field =
        match field.fbitfield with
        | Some 0 -> state (* skip the field, nothing to initialize *)
        | _ ->
          let new_offset = Field (field, NoOffset) in
          let offset = Cil.addOffset new_offset offset_orig in
          let nd = name_desc ^ "." ^ field.fname in
          let n = field.fname ^ "_" ^ name in
          add_offsetmap depth b nd n field.ftype offset typ_orig state
      in
      begin
        try
          List.fold_left treat_field state
            (Option.value ~default:[] compinfo.cfields)
        with Cil.SizeOfError (s, t) ->
          warn_unknown_size varinfo (s, t);
          bind_entire_loc Cvalue.V.top_int;
      end

    | TComp {cstruct=false} when Cil.is_fully_arithmetic typ ->
      reject_empty_struct b offset_orig typ;
      (* Union of arithmetic types *)
      bind_entire_loc Cvalue.V.top_int

    | TPtr _ when Parameters.ContextValidPointers.get () ->
      (* deep pointers map to NULL in this case *)
      bind_entire_loc Cvalue.V.singleton_zero

    | TBuiltin_va_list | TComp _ | TVoid | TPtr _ ->
      reject_empty_struct b offset_orig typ;
      (* variable arguments or union with non-arithmetic type
         or deep pointers *)
      (* first create a new varid and offsetmap for the
         "hidden location" *)
      let hidden_var_name =
        Cabs2cil.fresh_global ("WELL_"^name)
      in
      let hidden_var =
        Eva_utils.create_new_var hidden_var_name Cil_const.charType
      in
      hidden_var.vdescr <- Some (name_desc^"_WELL");
      let validity = Base.Known (Z.zero, Bit_utils.max_bit_address ()) in
      let hidden_base = Base.register_memory_var hidden_var validity in
      make_well hidden_base state (Lazy.force loc)
    | TNamed _ -> assert false
  in
  add_offsetmap
    0
    (Base.of_varinfo varinfo)
    varinfo.vname varinfo.vname varinfo.vtype NoOffset varinfo.vtype state
