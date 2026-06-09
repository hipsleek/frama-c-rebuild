(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Memory Model                                                       --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Cil_datatype
open Definitions
open Ctypes
open Lang
open Lang.F
open Memory
open Sigma

(* -------------------------------------------------------------------------- *)
(* --- Compound Loader                                                    --- *)
(* -------------------------------------------------------------------------- *)

let cluster () =
  Definitions.cluster ~id:"Compound" ~title:"Memory Compound Loader" ()

module type Model =
sig

  val name : string

  type loc
  val pretty : Format.formatter -> loc -> unit
  val sizeof : c_object -> term
  val field : loc -> fieldinfo -> loc
  val shift : loc -> c_object -> term -> loc

  val to_region_pointer : loc -> int * term
  val of_region_pointer : int -> c_object -> term -> loc

  val value_footprint: c_object -> loc -> domain
  val init_footprint: c_object -> loc -> domain

  val last : sigma -> c_object -> loc -> term

  val fresh : loc -> var list * loc
  val separated : loc -> term -> loc -> term -> pred

  val eqmem : Chunk.t -> term -> term -> loc -> term -> pred
  val memcpy : Chunk.t -> term -> loc -> term -> loc -> term -> term

  val load_int : sigma -> c_int -> loc -> term
  val load_float : sigma -> c_float -> loc -> term
  val load_pointer : sigma -> typ -> loc -> loc
  val load_init_atom : sigma -> c_object -> loc -> term

  val store_int : sigma -> c_int -> loc -> term -> Chunk.t * term
  val store_float : sigma -> c_float -> loc -> term -> Chunk.t * term
  val store_pointer : sigma -> typ -> loc -> term -> Chunk.t * term
  val store_init_atom : sigma -> c_object -> loc -> term -> Chunk.t * term

end

module Make (M : Model) =
struct

  let signature ft =
    let s = Sigma.create () in
    let xs = ref [] in
    let cs = ref [] in
    Domain.iter
      (fun c ->
         cs := c :: !cs ;
         xs := (Sigma.get s c) :: !xs ;
      ) ft ;
    List.rev !xs , List.rev !cs , s

  let domain obj loc =
    Domain.union
      (M.value_footprint obj loc)
      (M.init_footprint obj loc)

  let pp_rid fmt r = if r <> 0 then Format.fprintf fmt "_R%04x" r

  (* -------------------------------------------------------------------------- *)
  (* --- Frame Lemmas for Compound Access                                   --- *)
  (* -------------------------------------------------------------------------- *)

  let memories sigma chunks = List.map (Sigma.value sigma) chunks

  let frame_lemmas phi obj ?(length = F.e_one) loc params chunks =
    begin
      let prefix = Fun.debug phi in
      let s1 = Sigma.create () in
      let s2 = Sigma.create () in
      let v1 = e_fun phi (params @ memories s1 chunks) in
      let v2 = e_fun phi (params @ memories s2 chunks) in
      let n = F.e_mul length @@ M.sizeof obj in
      let eqm =
        F.p_all
          (fun c ->
             let m1 = Sigma.value s1 c in
             let m2 = Sigma.value s2 c in
             M.eqmem c m1 m2 loc n
          ) chunks in
      let def = F.p_imply eqm (F.p_equal v1 v2) in
      Definitions.define_lemma {
        l_kind = Admit ;
        l_name = Format.asprintf "%s_framed" prefix ;
        l_triggers = [] ;
        l_forall = F.p_vars def ;
        l_lemma = def ;
        l_cluster = cluster () ;
      }
    end

  (* -------------------------------------------------------------------------- *)
  (* ---  Loader utils                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  module COMP_KEY =
  struct
    type t = int * compinfo
    let compare (r,c) (r',c') = if r=r' then Compinfo.compare c c' else r-r'
    let pretty fmt (r,c) = Format.fprintf fmt "%d:%a" r Compinfo.pretty c
  end

  module ARRAY_KEY =
  struct
    type t = int * base * Matrix.t
    and base = I of c_int | F of c_float | P | C of compinfo
    let make r elt ds =
      let base = match elt with
        | C_int i -> I i
        | C_float f -> F f
        | C_pointer _ -> P
        | C_comp c -> C c
        | C_array _ -> raise (Invalid_argument "Wp.EqArray")
      in r, base , ds
    let key = function
      | I i -> Ctypes.i_name i
      | F f -> Ctypes.f_name f
      | P -> "ptr"
      | C c -> Lang.comp_id c
    let key_init = function
      | (I _ | F _ | P) as b -> key b ^ "_init"
      | C c -> Lang.comp_init_id c
    let obj = function
      | I i -> C_int i
      | F f -> C_float f
      | P -> C_pointer Cil_const.voidPtrType
      | C c -> C_comp c
    let tau = function
      | I _ -> Lang.t_int
      | F f -> Lang.t_float f
      | P -> Lang.t_addr ()
      | C c -> Lang.t_comp c
    let tau_init = function
      | I _ | F _ | P -> Lang.t_bool
      | C c -> Lang.t_init c
    let compare (r,a,p) (s,b,q) =
      if r = s then
        let cmp = String.compare (key a) (key b) in
        if cmp <> 0 then cmp else Matrix.compare p q
      else r - s
    let pretty fmt (r,a,ds) =
      Format.fprintf fmt "%s%a%a" (key a) pp_rid r Matrix.pp_suffix_id ds
  end

  module type LOAD_INFO = sig
    val kind : Lang.datakind
    val footprint : c_object -> M.loc -> domain
    val t_comp : compinfo -> Lang.tau
    val t_array : ARRAY_KEY.base -> Lang.tau
    val comp_id : compinfo -> string
    val array_id : ARRAY_KEY.base -> string
    val load : sigma -> c_object -> M.loc -> term
  end

  let fail _ _ _ = assert false

  module VALUE_LOAD_INFO = struct
    let kind = KValue
    let footprint = M.value_footprint
    let t_comp = Lang.t_comp
    let t_array = ARRAY_KEY.tau
    let comp_id = Lang.comp_id
    let array_id = ARRAY_KEY.key
    let load_rec = ref fail
    let load sigma = !load_rec sigma
  end

  module INIT_LOAD_INFO = struct
    let kind = KInit
    let footprint = M.init_footprint
    let t_comp = Lang.t_init
    let t_array = ARRAY_KEY.tau_init
    let comp_id = Lang.comp_init_id
    let array_id = ARRAY_KEY.key_init
    let load_rec = ref fail
    let load sigma = !load_rec sigma
  end

  (* -------------------------------------------------------------------------- *)
  (* ---  Compound Loader                                                   --- *)
  (* -------------------------------------------------------------------------- *)

  module COMP_GEN (Info : LOAD_INFO) = WpContext.Generator(COMP_KEY)
      (struct
        let name = M.name ^ ".COMP" ^ (if Info.kind = KInit then "INIT" else "")
        type key = int * compinfo
        type data = lfun * chunk list

        let generate (r,c) =
          let x = Lang.freshvar ~basename:"p" (Lang.t_addr()) in
          let v = e_var x in
          let obj = C_comp c in
          let loc = M.of_region_pointer r obj v in (* t_pointer -> loc *)
          let domain = Info.footprint obj loc in
          let result = Info.t_comp c in
          let lfun =
            Lang.generated_f ~context:true ~result "Load%a_%s"
              pp_rid r (Info.comp_id c) in
          let xms,chunks,sigma = signature domain in
          let prms = x :: xms in
          let dfun =
            match c.cfields with
            | None -> Definitions.Logic result
            | Some fields ->
              let def = List.map
                  (fun f ->
                     let fd = cfield ~kind:Info.kind f in
                     let ft = object_of f.ftype in
                     let fv = Info.load sigma ft (M.field loc f) in
                     let pr = F.e_apply (F.e_lambda prms fv) in
                     F.set_builtin_field lfun fd pr ;
                     fd,fv
                  ) fields
              in Definitions.Function( result , Def , e_record def )
          in
          Definitions.define_symbol {
            d_lfun = lfun ; d_types = 0 ;
            d_params = prms ;
            d_definition = dfun ;
            d_cluster = cluster () ;
          } ;
          frame_lemmas lfun obj loc [v] chunks ;
          lfun , chunks

        let compile = Lang.local generate
      end)

  module COMP = COMP_GEN(VALUE_LOAD_INFO)
  module COMP_INIT = COMP_GEN(INIT_LOAD_INFO)

  (* -------------------------------------------------------------------------- *)
  (* ---  Array Loader                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  module ARRAY_GEN(Info: LOAD_INFO) = WpContext.Generator(ARRAY_KEY)
      (struct
        open Matrix
        let name = M.name ^ ".ARRAY" ^ (if Info.kind=KInit then "INIT" else "")
        type key = ARRAY_KEY.t
        type data = lfun * chunk list

        let generate (r,a,ds) =
          let x = Lang.freshvar ~basename:"p" (Lang.t_addr()) in
          let v = e_var x in
          let obj = ARRAY_KEY.obj a in
          let loc = M.of_region_pointer r obj v in (* t_pointer -> loc *)
          let domain = Info.footprint obj loc in
          let result = Matrix.cc_tau (Info.t_array a) ds in
          let lfun =
            Lang.generated_f ~result ~context:true "Array%a_%s%a"
              pp_rid r (Info.array_id a) Matrix.pp_suffix_id ds in
          let prefix = Lang.Fun.debug lfun in
          let name = prefix ^ "_access" in
          let xms,chunks,sigma = signature domain in
          let env = Matrix.cc_env ds in
          let prms = x :: env.size_var @ xms in
          let phi = e_fun lfun (v :: env.size_val @ List.map e_var xms) in
          let va = List.fold_left e_get phi env.index_val in
          let ofs = e_sum env.index_offset in
          let vm = Info.load sigma obj (M.shift loc obj ofs) in
          let lemma = p_hyps env.index_range (p_equal va vm) in
          let cluster = cluster () in
          Definitions.define_symbol {
            d_lfun = lfun ;
            d_types = 0 ;
            d_params = prms ;
            d_definition = Logic result ;
            d_cluster = cluster ;
          } ;
          Definitions.define_lemma {
            l_kind = Admit ;
            l_name = name ;
            l_forall = F.p_vars lemma ;
            l_triggers = [[Trigger.of_term va]] ;
            l_lemma = lemma ;
            l_cluster = cluster ;
          } ;
          let pr = F.e_lambda (prms @ env.index_var) vm in
          let nk = List.length env.index_var in
          Lang.F.set_builtin_get lfun
            (fun es ks ->
               if List.length ks = nk then
                 F.e_apply pr (es @ ks)
               else
                 raise Not_found
            ) ;
          begin
            match env.length with
            | None -> ()
            | Some length ->
              let ns = List.map F.e_var env.size_var in
              frame_lemmas lfun obj ~length loc (v::ns) chunks
          end ;
          lfun , chunks

        let compile = Lang.local generate
      end)

  module ARRAY = ARRAY_GEN(VALUE_LOAD_INFO)
  module ARRAY_INIT = ARRAY_GEN(INIT_LOAD_INFO)

  (* -------------------------------------------------------------------------- *)
  (* --- Loaders                                                            --- *)
  (* -------------------------------------------------------------------------- *)

  module LOADER_GEN
      (ATOM: sig
         val load_int : sigma -> c_int -> M.loc -> term
         val load_float : sigma -> c_float -> M.loc -> term
         val load_pointer : sigma -> typ -> M.loc -> term
       end)
      (COMP: sig
         val get : (int*compinfo) -> (lfun * chunk list)
       end)
      (ARRAY: sig
         val get : (int*ARRAY_KEY.base*Matrix.t) -> (lfun * chunk list)
       end) =
  struct

    let load_comp sigma comp loc =
      let r , p = M.to_region_pointer loc in
      let f , m = COMP.get (r,comp) in
      F.e_fun f (p :: memories sigma m)

    let load_array sigma a loc =
      let r , p = M.to_region_pointer loc in
      let e , ns = Ctypes.array_dimensions a in
      let ds = Matrix.of_dims ns in
      let f , m = ARRAY.get @@ ARRAY_KEY.make r e ds in
      F.e_fun f (p :: Matrix.cc_dims ns @ memories sigma m)

    let load sigma obj loc =
      match obj with
      | C_int i -> ATOM.load_int sigma i loc
      | C_float f -> ATOM.load_float sigma f loc
      | C_pointer t -> ATOM.load_pointer sigma t loc
      | C_comp c -> load_comp sigma c loc
      | C_array a -> load_array sigma a loc
  end

  module VALUE_LOADER =
    LOADER_GEN
      (struct
        let load_int = M.load_int
        let load_float = M.load_float
        let load_pointer sigma t loc =
          snd @@ M.to_region_pointer @@ M.load_pointer sigma t loc
      end)
      (COMP)(ARRAY)

  let load_comp = VALUE_LOADER.load_comp
  let load_array = VALUE_LOADER.load_array
  let load_value = VALUE_LOADER.load

  let () = VALUE_LOAD_INFO.load_rec := load_value

  let load sigma obj loc =
    let open Memory in
    match obj with
    | C_int i -> Val (M.load_int sigma i loc)
    | C_float f -> Val (M.load_float sigma f loc)
    | C_pointer t -> Loc (M.load_pointer sigma t loc)
    | C_comp c -> Val (load_comp sigma c loc)
    | C_array a -> Val (load_array sigma a loc)

  (* -------------------------------------------------------------------------- *)
  (* --- Initialized                                                        --- *)
  (* -------------------------------------------------------------------------- *)

  let isinitrec = ref (fun _ _ _ -> assert false)

  module IS_INIT_COMP = WpContext.Generator(COMP_KEY)
      (struct
        let name = M.name ^ ".IS_INIT_COMP"
        type key = int * compinfo
        type data = lfun * chunk list

        let generate (r,c) =
          let x = Lang.freshvar ~basename:"p" (Lang.t_addr()) in
          let obj = C_comp c in
          let loc = M.of_region_pointer r obj (e_var x) in
          let domain = M.init_footprint obj loc in
          let cluster = cluster () in
          (* Is_init: structural definition *)
          let name =
            Format.asprintf "Is%s%a" (Lang.comp_init_id c) pp_rid r
          in
          let lfun = Lang.generated_p name in
          let xms,chunks,sigma = signature domain in
          let params = x :: xms in
          let def = match c.cfields with
            | None -> Logic Lang.t_prop
            | Some fields ->
              let def = p_all
                  (fun f -> !isinitrec sigma (object_of f.ftype) (M.field loc f))
                  fields
              in
              Predicate(Def, def)
          in
          Definitions.define_symbol {
            d_lfun = lfun ; d_types = 0 ;
            d_params = params ;
            d_definition = def ;
            d_cluster = cluster ;
          } ;
          frame_lemmas lfun obj loc [e_var x] chunks ;
          lfun , chunks

        let compile = Lang.local generate
      end)

  module IS_ARRAY_INIT = WpContext.Generator(ARRAY_KEY)
      (struct
        open Matrix
        let name = M.name ^ ".IS_ARRAY_INIT"
        type key = ARRAY_KEY.t
        type data = lfun * chunk list

        let generate (r,a,ds) =
          let x = Lang.freshvar ~basename:"p" (Lang.t_addr()) in
          let v = e_var x in
          let obj = ARRAY_KEY.obj a in
          let loc = M.of_region_pointer r obj v in
          let domain = M.init_footprint obj loc in
          let name = Format.asprintf "IsInitArray%a_%s%a"
              pp_rid r (ARRAY_KEY.key a) Matrix.pp_suffix_id ds in
          let lfun = Lang.generated_p name in
          let xmem,chunks,sigma = signature domain in
          let env = Matrix.cc_env ds in
          let params = x :: env.size_var @ xmem in
          let ofs = e_sum env.index_offset in
          let vm = !isinitrec sigma obj (M.shift loc obj ofs) in
          let def = p_forall env.index_var (p_hyps env.index_range vm) in
          let cluster = cluster () in
          (* Is_init: structural definition *)
          Definitions.define_symbol {
            d_lfun = lfun ; d_types = 0 ;
            d_params = params ;
            d_definition = Predicate (Def, def) ;
            d_cluster = cluster ;
          } ;
          begin
            match env.length with
            | None -> ()
            | Some length ->
              let ns = List.map F.e_var env.size_var in
              frame_lemmas lfun obj ~length loc (v::ns) chunks
          end ;
          lfun , chunks

        let compile = Lang.local generate
      end)

  let initialized_comp sigma comp loc =
    let r , p = M.to_region_pointer loc in
    let f , m = IS_INIT_COMP.get (r,comp) in
    F.p_call f (p :: memories sigma m)

  let initialized_array sigma ainfo loc =
    let r , p = M.to_region_pointer loc in
    let e , ns = Ctypes.array_dimensions ainfo in
    let ds = Matrix.of_dims ns in
    let f , m = IS_ARRAY_INIT.get @@ ARRAY_KEY.make r e ds in
    F.p_call f (p :: Matrix.cc_dims ns @ memories sigma m)

  let initialized_loc sigma obj loc =
    match obj with
    | C_int _ | C_float _ | C_pointer _ ->
      p_bool (M.load_init_atom sigma obj loc)
    | C_comp ci -> initialized_comp sigma ci loc
    | C_array a -> initialized_array sigma a loc

  let () = isinitrec := initialized_loc

  let initialized sigma = function
    | Rloc(obj, loc) -> initialized_loc sigma obj loc
    | Rrange(loc, obj, Some low, Some up) ->
      let x = Lang.freshvar ~basename:"i" Lang.t_int in
      let v = e_var x in
      let hyps = [ p_leq low v ; p_leq v up] in
      let loc = M.shift loc obj v in
      p_forall [x] (p_hyps hyps (initialized_loc sigma obj loc))
    | Rrange(_l, _, low, up) ->
      Wp_parameters.abort ~current:true
        "Invalid infinite range @[<hov 2>+@,(%a@,..%a)@]"
        Vset.pp_bound low Vset.pp_bound up

  module INIT_LOADER =
    LOADER_GEN
      (struct
        let load_int sigma ikind = M.load_init_atom sigma (C_int ikind)
        let load_float sigma fkind = M.load_init_atom sigma (C_float fkind)
        let load_pointer sigma typ = M.load_init_atom sigma (C_pointer typ)
      end)(COMP_INIT)(ARRAY_INIT)

  let load_init = INIT_LOADER.load
  let () = INIT_LOAD_INFO.load_rec := load_init

  (* -------------------------------------------------------------------------- *)
  (* --- Mem Copies \ Havocs                                                --- *)
  (* -------------------------------------------------------------------------- *)

  let update (s : sigma sequence) ?(init=false) obj loc ?src ?(length = e_one) () =
    let ps = ref [] in
    let size = F.e_mul length @@ M.sizeof obj in
    let domain =
      if init then M.init_footprint obj loc else M.value_footprint obj loc in
    Domain.iter
      (fun chunk ->
         let m_pre = Sigma.value s.pre chunk in
         let m_post = Sigma.value s.post chunk in
         let m_copied =
           match src with
           | None ->
             let tau = Chunk.tau_of_chunk chunk in
             let basename = Chunk.basename_of_chunk chunk ^ "_undef" in
             let m_undef = F.e_var (Lang.freshvar ~basename tau) in
             M.memcpy chunk m_pre loc m_undef loc size
           | Some src ->
             M.memcpy chunk m_pre loc m_pre src size
         in
         ps := Set(m_post,m_copied) :: !ps
      ) domain ; !ps

  (* -------------------------------------------------------------------------- *)
  (* --- Stored & Copied                                                    --- *)
  (* -------------------------------------------------------------------------- *)

  let stored_chunk seq (c,m) = [ Set(Sigma.value seq.post c,m) ]

  let stored seq obj loc value =
    match obj with
    | C_int i -> stored_chunk seq @@ M.store_int seq.pre i loc value
    | C_float f -> stored_chunk seq @@ M.store_float seq.pre f loc value
    | C_pointer t -> stored_chunk seq @@ M.store_pointer seq.pre t loc value
    | C_comp _ | C_array _ ->
      Set(load_value seq.post obj loc, value) :: update seq obj loc ()

  let stored_init seq obj loc value =
    match obj with
    | C_int _ | C_float _ | C_pointer _ ->
      stored_chunk seq @@ M.store_init_atom seq.pre obj loc value
    | C_comp _ | C_array _ ->
      let v_tgt = load_init seq.post obj loc in
      Set(v_tgt,value) :: update seq ~init:true obj loc ()

  let copied seq obj loc src =
    match obj with
    | C_int _ | C_float _ | C_pointer _ ->
      stored seq obj loc @@ load_value seq.pre obj src
    | C_comp _ | C_array _ ->
      let v_src = load_value seq.pre obj src in
      let v_tgt = load_value seq.post obj loc in
      let src = if Wp_parameters.Havoc.get () then None else Some src in
      Set(v_tgt,v_src) :: update seq obj loc ?src ()

  let copied_init seq obj loc src =
    match obj with
    | C_int _ | C_float _ | C_pointer _ ->
      stored_init seq obj loc @@ load_init seq.pre obj src
    | C_comp _ | C_array _ ->
      let v_src = load_init seq.pre obj src in
      let v_tgt = load_init seq.post obj loc in
      let src = if Wp_parameters.Havoc.get () then None else Some src in
      Set(v_tgt,v_src) :: update seq ~init:true obj loc ?src ()

  (* -------------------------------------------------------------------------- *)
  (* --- Assigned                                                           --- *)
  (* -------------------------------------------------------------------------- *)

  let assigned_loc seq obj loc =
    match obj with
    | C_int _ | C_float _ | C_pointer _ ->
      let value = Lang.freshvar ~basename:"v" (Lang.tau_of_object obj) in
      let init = Lang.freshvar ~basename:"i" (Lang.init_of_object obj) in
      stored seq obj loc (e_var value) @
      stored_init seq obj loc (e_var init)
    | C_comp _ | C_array _ ->
      update seq obj loc () @
      update seq ~init:true obj loc ()

  let assigned_range seq obj l a b =
    let loc = M.shift l obj a in
    let length = e_range a b in
    update seq obj loc ~length () @
    update seq ~init:true obj loc ~length ()

  let assigned seq obj sloc =
    match sloc with
    | Sloc loc -> assigned_loc seq obj loc
    | Sdescr(xs,loc,condition) ->
      let ps = ref [] in
      Domain.iter
        (fun c ->
           let m1 = Sigma.value seq.pre c in
           let m2 = Sigma.value seq.post c in
           let n = M.sizeof obj in
           let ys,q = M.fresh loc in
           let sep = M.separated q e_one loc n in
           let out = F.p_forall xs (p_imply condition sep) in
           let eqm = M.eqmem c m1 m2 q e_one in
           ps := Assert (F.p_forall ys @@ p_imply out eqm) :: !ps
        ) (domain obj loc) ;
      !ps
    | Sarray(loc,obj,n) ->
      assigned_range seq obj loc e_zero (e_int (n-1))
    | Srange(loc,obj,u,v) ->
      let a = match u with Some a -> a | None -> e_zero in
      let b = match v with Some b -> b | None -> M.last seq.pre obj loc in
      assigned_range seq obj loc a b

  (* -------------------------------------------------------------------------- *)

end
