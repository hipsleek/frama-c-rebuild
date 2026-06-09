(**************************************************************************)
(*                                                                        *)
(*  SPDX-License-Identifier LGPL-2.1                                      *)
(*  Copyright (C)                                                         *)
(*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  *)
(*                                                                        *)
(**************************************************************************)

open Format

let pp_type fmt (signed, size) =
  let prefix = if signed then 's' else 'u' in
  fprintf fmt "%cint%d" prefix size

let pp_use fmt (m, alias) =
  let pp_alias fmt = function
    | Some alias -> fprintf fmt " as %s" alias
    | None -> ()
  in
  fprintf fmt "use %s%a" m pp_alias alias

let pp_use_list fmt l =
  fprintf fmt "%a" (pp_print_list ~pp_sep:pp_print_cut pp_use) l

let all_sizes = [ 8 ; 16 ; 32 ; 64 ; 128 ]
let all_types = [ (false, 8) ; (false, 16) ; (false, 32) ; (false, 64) ;
                  (false, 128)
                ; (true , 8) ; (true , 16) ; (true , 32) ; (true , 64) ;
                  (true, 128) ]

(* -------------------------------------------------------------------------- *)
(* ---  Value Encode/Decode                                               --- *)
(* -------------------------------------------------------------------------- *)

let value_codec_preamble fmt () =
  fprintf fmt "%a@," pp_use_list
    [ "int.Int", None
    ; "int.ComputerDivision", None
    ; "frama_c_wp.cint.Cint", None
    ; "frama_c_wp.sequence.Seq", None ]

let value_codec_encode_unsigned fmt size =
  let consbyte ~last fmt n =
    if n = 1
    then fprintf fmt "(L.Cons (mod v 0x100)"
    else fprintf fmt "(L.Cons (mod (div v 0x%x) 0x100)" (1 lsl (8 * (n - 1))) ;

    if last then fprintf fmt " L.Nil%s" (String.init n (fun _ -> ')')) ;
    fprintf fmt "@,"
  in
  let rec all_consbytes n fmt m =
    if n = m
    then consbyte ~last:true fmt n
    else begin consbyte ~last:false fmt n ; all_consbytes (n+1) fmt m end
  in
  fprintf fmt "@[<v 2>function encode_%a (v: int) : seq int =@," pp_type (false, size) ;
  fprintf fmt "%a@]@," (all_consbytes 1) (size / 8)

let value_codec_encode_signed fmt size =
  fprintf fmt "@[<v 2>function encode_%a (v: int) : seq int =@," pp_type (true, size) ;
  fprintf fmt "encode_%a (to_%a v)@,@]@," pp_type (false, size) pp_type (false, size)

let value_codec_encode fmt size =
  value_codec_encode_unsigned fmt size ;
  value_codec_encode_signed fmt size

let value_codec_decode_unsigned fmt size =
  let rec mline n fmt stop =
    if n = stop
    then fprintf fmt "L.Nil"
    else fprintf fmt "L.Cons b%d (%a)" n (mline (n + 1)) stop
  in
  let rec rline n fmt stop =
    if n = stop - 1
    then fprintf fmt "b%d * 0x%x" n (1 lsl (8 * n))
    else fprintf fmt "b%d * 0x%x + %a" n (1 lsl (8 * n)) (rline (n + 1)) stop
  in
  let case fmt stop =
    fprintf fmt "| %a ->@," (mline 0) stop ;
    fprintf fmt "  %a@," (rline 0) stop ;
  in
  fprintf fmt "@[<v 2>function decode_%a (s: seq int) : int =@," pp_type (false, size) ;
  fprintf fmt "match s with@," ;
  fprintf fmt "%a" case (size / 8) ;
  fprintf fmt "| _ -> 0@," ;
  fprintf fmt "end@,@]@,"

let value_codec_decode_signed fmt size =
  fprintf fmt "@[<v 2>function decode_%a (s: seq int) : int =@," pp_type (true, size) ;
  fprintf fmt "to_%a (decode_%a s)@,@]@," pp_type (true, size) pp_type (false, size)

let value_codec_decode fmt size =
  value_codec_decode_unsigned fmt size ;
  value_codec_decode_signed fmt size

let value_codec_lemma_decode_encode fmt decoded encoded =
  let guard fmt () =
    fprintf fmt "is_%a v" pp_type encoded
  in
  let same = match decoded, encoded with (sd, _), (se, _) -> sd = se in
  let decenc fmt () =
    fprintf fmt "decode_%a (encode_%a v)"
      pp_type decoded
      pp_type encoded
  in
  let eq fmt () =
    fprintf fmt "%a = %sv"
      decenc ()
      (if same then "" else asprintf "to_%a " pp_type decoded)
  in
  fprintf fmt "@[<v 2>lemma decode_%a_encode_%a:@,"
    pp_type decoded pp_type encoded ;
  fprintf fmt "forall v: int [%a].@,%a -> %a@,@]@," decenc () guard () eq ()

let value_codec_lemma_decode_encode fmt size =
  value_codec_lemma_decode_encode fmt (false, size) (false, size) ;
  value_codec_lemma_decode_encode fmt (true,  size) (false, size) ;
  value_codec_lemma_decode_encode fmt (false, size) (true , size) ;
  value_codec_lemma_decode_encode fmt (true,  size) (true , size)

let value_codec_all_symbols fmt size =
  value_codec_decode fmt size ;
  value_codec_encode fmt size ;
  value_codec_lemma_decode_encode fmt size

let value_codec fmt () =
  fprintf fmt "%a@," value_codec_preamble () ;
  List.iter (fprintf fmt "%a" value_codec_all_symbols) all_sizes

(* -------------------------------------------------------------------------- *)
(* ---  Init Encode/Decode                                                --- *)
(* -------------------------------------------------------------------------- *)

let init_codec_preamble fmt () =
  fprintf fmt "%a@," pp_use_list
    [ "bool.Bool", None
    ; "frama_c_wp.sequence.Seq", None ]

let init_codec_decode fmt size =
  let rec mline n fmt stop =
    if n = stop
    then fprintf fmt "L.Nil"
    else fprintf fmt "L.Cons b%d (%a)" n (mline (n + 1)) stop
  in
  let rec rline n fmt stop =
    if n = stop - 1
    then fprintf fmt "b%d" n
    else fprintf fmt "b%d && %a" n (rline (n + 1)) stop
  in
  let case fmt stop =
    fprintf fmt "| %a ->@," (mline 0) stop ;
    fprintf fmt "  %a@," (rline 0) stop
  in
  fprintf fmt "@[<v 2>function decode_init%d (s: seq bool) : bool =@," size ;
  fprintf fmt "match s with@," ;
  fprintf fmt "%a" case (size / 8) ;
  fprintf fmt "| _ -> false@," ;
  fprintf fmt "end@,@]@,"

let init_codec_encode fmt size =
  fprintf fmt "@[<v 2>function encode_init%d (v: bool) : seq bool =@," size ;
  fprintf fmt "create v %d" (size / 8) ;
  fprintf fmt "@,@]@,"

let init_codec_lemma_decode_encode fmt size =
  let decenc fmt () =
    fprintf fmt "decode_init%d (encode_init%d v)" size size
  in
  let eq fmt () = fprintf fmt "%a = v" decenc () in
  fprintf fmt "@[<v 2>lemma decode_init%d_encode_init%d:@," size size ;
  fprintf fmt "forall v: bool [%a].@,%a@,@]@," decenc () eq ()


let init_codec_all_symbols fmt size =
  init_codec_decode fmt size ;
  init_codec_encode fmt size ;
  init_codec_lemma_decode_encode fmt size

let init_codec fmt () =
  fprintf fmt "%a@," init_codec_preamble () ;
  List.iter (fprintf fmt "%a" init_codec_all_symbols) all_sizes

(* -------------------------------------------------------------------------- *)
(* ---  Offset                                                            --- *)
(* -------------------------------------------------------------------------- *)

let offset fmt () =
  fprintf fmt {|
  use int.Int
  type offset = int

  predicate sepoffset (po: offset) (lp: int) (qo: offset) (lq: int) =
    qo + lq <= po \/ po + lp <= qo
|}

(* -------------------------------------------------------------------------- *)
(* ---  RWBytes                                                           --- *)
(* -------------------------------------------------------------------------- *)

let rwbytes_preamble fmt () =
  fprintf fmt "%a@,@," pp_use_list
    [ "int.Int", None ; "map.Map", Some "M" ;
      "frama_c_wp.sequence.Seq", Some "S" ; "Offset", None] ;
  fprintf fmt {|
  type seq   'a = S.seq 'a
  type block 'a = M.map int 'a

  function bwrite_seq (b:block 'a) (o: int) (s: seq 'a) : block 'a =
    match s with
    | S.L.Nil -> b
    | S.L.Cons h tl -> M.(set (bwrite_seq b (o+1) tl) o h)
    end

  predicate beq_blocks (b1 b2: block 'a) (o: int) (l: int) =
    forall i: int. o <= i < o + l -> M.(get b1 i) = M.(get b2 i)
|}

let rwbytes_write (le: bool) fmt size =
  let rec mline n max fmt stop =
    if n = stop
    then fprintf fmt "%s" (if max = stop then "_" else "S.L.Nil")
    else fprintf fmt "S.L.Cons b%d (%a)" n (mline (n + 1) max) stop
  in
  let rec rline n fmt stop =
    let pp_offset fmt n =
      let n = if le then n else (size / 8) - (n + 1) in
      match n with
      | 0 -> fprintf fmt "o"
      | n -> fprintf fmt "(o+%d)" n
    in
    if n = stop
    then fprintf fmt "b"
    else fprintf fmt "(set %a %a b%d)" (rline (n + 1)) stop pp_offset n n
  in
  let case stop fmt max =
    fprintf fmt "| %a ->@," (mline 0 max) stop ;
    fprintf fmt "  M.%a@," (rline 0) stop ;
  in
  fprintf fmt "@[<v 2>function bwrite_%dbits (b: block 'a) (o: int) (s: seq 'a) : block 'a =@," size ;
  fprintf fmt "match s with@," ;
  fprintf fmt "%a" (case (size / 8)) (size / 8) ;
  fprintf fmt "| _ -> b@," ;
  fprintf fmt "end@,@]@,"

let rwbytes_read (le: bool) fmt size =
  let consbyte ~last fmt n =
    let o = if le then n else (size / 8) - (n - 1) in

    if o = 1
    then fprintf fmt "(S.L.Cons M.(b[o  ])"
    else fprintf fmt "(S.L.Cons M.(b[o+%d])" (o - 1) ;

    if last then fprintf fmt " S.L.Nil%s" (String.init n (fun _ -> ')')) ;
    fprintf fmt "@,"
  in
  let rec all_consbytes n fmt m =
    if n = m
    then consbyte ~last:true fmt n
    else begin
      consbyte ~last:false fmt n ;
      all_consbytes (n+1) fmt m
    end
  in
  fprintf fmt "@[<v 2>function bread_%dbits (b: block 'a) (o: int) : seq 'a =@," size ;
  fprintf fmt "%a@]@," (all_consbytes 1) (size / 8)

let rwbytes_read_write_eq fmt size =
  let guard fmt () =
    fprintf fmt "S.length s = %d" (size / 8)
  in
  let read_write fmt () =
    fprintf fmt "bread_%dbits (bwrite_%dbits b o s) o" size size
  in
  let eq fmt () = fprintf fmt "%a = s" read_write () in
  fprintf fmt "@[<v 2>lemma bread_%dbits_bwrite_%dbits_eq:@," size size ;
  fprintf fmt "forall b: block 'a, o: int, s: seq 'a [%a].@," read_write () ;
  fprintf fmt "%a ->@,  %a@]@,@," guard () eq ()

let rwbytes_read_bwrite_seq_sep fmt rsize =
  let brsize = rsize / 8 in
  let guard fmt () =
    fprintf fmt "sepoffset ow (S.length s) or %d" brsize in
  let read_write fmt () =
    fprintf fmt "bread_%dbits (bwrite_seq b ow s) or" rsize
  in
  let eq fmt () = fprintf fmt "%a = bread_%dbits b or" read_write () rsize in
  fprintf fmt "@[<v 2>let rec lemma bread_%dbits_bwrite_seq_sep " rsize ;
  fprintf fmt "(b: block 'a)(or ow: int)(s: seq 'a)@," ;
  fprintf fmt "requires { %a }@," guard() ;
  fprintf fmt "ensures  { %a }@,@]" eq () ;
  fprintf fmt "@[<v 2>= match s with@," ;
  fprintf fmt "| S.L.Nil -> ()@," ;
  fprintf fmt "| S.L.Cons _ tl -> bread_%dbits_bwrite_seq_sep b or (ow + 1) tl@," rsize ;
  fprintf fmt "end@]@,@,"

let rwbytes_read_write_sep fmt rsize wsize =
  let brsize = rsize / 8 and bwsize = wsize / 8 in
  let guard fmt () =
    fprintf fmt "sepoffset ow %d or %d" bwsize brsize in
  let read_write fmt () =
    fprintf fmt "bread_%dbits (bwrite_%dbits b ow s) or" rsize wsize
  in
  let eq fmt () = fprintf fmt "%a = bread_%dbits b or" read_write () rsize in
  fprintf fmt "@[<v 2>lemma bread_%dbits_bwrite_%dbits_sep:@," rsize wsize ;
  fprintf fmt "forall b: block 'a, or ow: int, s: seq 'a [%a].@," read_write () ;
  fprintf fmt "%a ->@,  %a@]@,@," guard () eq ()

let rwbytes_all_lemmas fmt size =
  rwbytes_read_write_eq fmt size ;
  rwbytes_read_bwrite_seq_sep fmt size ;
  List.iter (rwbytes_read_write_sep fmt size) all_sizes

let rwbytes fmt le =
  fprintf fmt "%a@," rwbytes_preamble () ;
  List.iter (fprintf fmt "%a" (rwbytes_read le)) all_sizes ;
  List.iter (fprintf fmt "%a" (rwbytes_write le)) all_sizes ;
  List.iter (fprintf fmt "%a" rwbytes_all_lemmas) all_sizes

(* -------------------------------------------------------------------------- *)
(* ---  ValueBlockRW                                                      --- *)
(* -------------------------------------------------------------------------- *)

let value_blockrw_preamble fmt () =
  fprintf fmt "%a@,@," pp_use_list
    [ "int.Int", None
    ; "frama_c_wp.cint.Cint", None
    ; "ValueCodec", None
    ; "Offset", None
    ; "RWBytes", None
    ] ;
  fprintf fmt "type vblock = block int@,"

let value_blockrw_write fmt ((_, size) as t) =
  fprintf fmt "@[<v 2>function bwrite_%a (b: vblock) (o: int) (v: int) : vblock =@," pp_type t;
  fprintf fmt "bwrite_%dbits b o (encode_%a v)@,@]@," size pp_type t

let value_blockrw_read fmt ((_, size) as t) =
  fprintf fmt "@[<v 2>function bread_%a (b: vblock) (o: int) : int =@," pp_type t ;
  fprintf fmt "decode_%a (bread_%dbits b o)@,@]@," pp_type t size

let value_blockrw_read_write_eq_same fmt t =
  let guard fmt () = fprintf fmt "is_%a v" pp_type t in
  let read_write fmt () =
    fprintf fmt "bread_%a (bwrite_%a b o v) o" pp_type t pp_type t
  in
  let eq fmt () = fprintf fmt "%a = v" read_write () in
  fprintf fmt "@[<v 2>lemma bread_%a_bwrite_%a_eq:@," pp_type t pp_type t ;
  fprintf fmt "forall b: vblock, o: int, v: int [%a].@," read_write () ;
  fprintf fmt "%a ->@,  %a@,@]@," guard () eq ()

let value_blockrw_read_write_eq_opposite fmt rt =
  let wt = not @@ fst rt, snd rt in
  let guard fmt () = fprintf fmt "is_%a v" pp_type wt in
  let read_write fmt () =
    fprintf fmt "bread_%a (bwrite_%a b o v) o" pp_type rt pp_type wt
  in
  let eq fmt () = fprintf fmt "%a = to_%a v" read_write () pp_type rt in
  fprintf fmt "@[<v 2>lemma bread_%a_bwrite_%a_eq:@," pp_type rt pp_type wt ;
  fprintf fmt "forall b: vblock, o: int, v: int [%a].@," read_write () ;
  fprintf fmt "%a ->@,  %a@,@]@," guard () eq ()

let value_blockrw_read_write_eq fmt t =
  value_blockrw_read_write_eq_same fmt t ;
  value_blockrw_read_write_eq_opposite fmt t

let value_blockrw_read_write_sep fmt rt wt =
  let guard fmt () =
    fprintf fmt "sepoffset ow %d or %d" ((snd wt) / 8) ((snd rt) / 8) in
  let read_write fmt () =
    fprintf fmt "bread_%a (bwrite_%a b ow v) or" pp_type rt pp_type wt
  in
  let eq fmt () = fprintf fmt "%a = bread_%a b or" read_write () pp_type rt in
  fprintf fmt "@[<v 2>lemma bread_%a_bwrite_%a_sep:@," pp_type rt pp_type wt ;
  fprintf fmt "forall b: vblock, or ow: int, v: int [%a].@," read_write () ;
  fprintf fmt "%a ->@,  %a@,@]@," guard () eq ()

let value_blockrw_read_copy_sep fmt rt =
  let guard fmt () =
    fprintf fmt "sepoffset or %d ho (Seq.length u)" ((snd rt) / 8) in
  let read_copy fmt () =
    fprintf fmt "bread_%a (bwrite_seq b ho u) or" pp_type rt
  in
  let eq fmt () = fprintf fmt "%a = bread_%a b or" read_copy () pp_type rt in
  fprintf fmt "@[<v 2>lemma bread_%a_bcopy_sep:@," pp_type rt ;
  fprintf fmt "forall b: vblock, u: S.seq int, or ho: int [%a].@," read_copy () ;
  fprintf fmt "%a ->@,  %a@,@]@," guard () eq ()

let value_blockrw_all_lemmas fmt size =
  value_blockrw_read_write_eq fmt size ;
  value_blockrw_read_copy_sep fmt size ;
  List.iter (value_blockrw_read_write_sep fmt size) all_types

let value_blockrw fmt () =
  fprintf fmt "%a@," value_blockrw_preamble () ;
  List.iter (fprintf fmt "%a" value_blockrw_read) all_types ;
  List.iter (fprintf fmt "%a" value_blockrw_write) all_types ;
  List.iter (fprintf fmt "%a" value_blockrw_all_lemmas) all_types

(* -------------------------------------------------------------------------- *)
(* ---  InitBlockRW                                                       --- *)
(* -------------------------------------------------------------------------- *)

let init_blockrw_preamble fmt () =
  fprintf fmt "%a@,@," pp_use_list
    [ "bool.Bool", None
    ; "int.Int", None
    ; "InitCodec", None
    ; "Offset", None
    ; "RWBytes", None
    ] ;
  fprintf fmt "type iblock = block bool@,@,"

let init_blockrw_write fmt size =
  fprintf fmt "@[<v 2>function bwrite_init%d (b: iblock) (o: int) (init: bool) : iblock =@," size;
  fprintf fmt "bwrite_%dbits b o (encode_init%d init)@,@]@," size size

let init_blockrw_read fmt size =
  fprintf fmt "@[<v 2>function bread_init%d (b: iblock) (o: int) : bool =@," size ;
  fprintf fmt "decode_init%d (bread_%dbits b o)@,@]@," size size

let init_blockrw_read_write_eq fmt size =
  let read_write fmt () =
    fprintf fmt "bread_init%d (bwrite_init%d b o init) o" size size
  in
  let eq fmt () = fprintf fmt "%a = init" read_write () in
  fprintf fmt "@[<v 2>lemma bread_init%d_bwrite_init%d_eq:@," size size ;
  fprintf fmt "forall b: iblock, o: int, init: bool [%a].@," read_write () ;
  fprintf fmt "%a@,@]@," eq ()

let init_blockrw_read_write_sep fmt rsize wsize =
  let guard fmt () =
    fprintf fmt "sepoffset ow %d or %d" (wsize / 8) (rsize / 8) in
  let read_write fmt () =
    fprintf fmt "bread_init%d (bwrite_init%d b ow init) or" rsize wsize
  in
  let eq fmt () = fprintf fmt "%a = bread_init%d b or" read_write () rsize in
  fprintf fmt "@[<v 2>lemma bread_init%d_bwrite_init%d_sep:@," rsize wsize ;
  fprintf fmt "forall b: iblock, or ow: int, init: bool [%a].@," read_write () ;
  fprintf fmt "%a ->@,  %a@,@]@," guard () eq ()

let init_blockrw_read_copy_sep fmt rsize =
  let guard fmt () =
    fprintf fmt "sepoffset or %d ho (Seq.length u)" (rsize / 8) in
  let read_copy fmt () =
    fprintf fmt "bread_init%d (bwrite_seq b ho u) or" rsize
  in
  let eq fmt () = fprintf fmt "%a = bread_init%d b or" read_copy () rsize in
  fprintf fmt "@[<v 2>lemma bread_init%d_bcopy_sep:@," rsize ;
  fprintf fmt "forall b: iblock, u: S.seq bool, or ho: int [%a].@," read_copy () ;
  fprintf fmt "%a ->@,  %a@,@]@," guard () eq ()

let init_blockrw_all_lemmas fmt size =
  init_blockrw_read_write_eq fmt size ;
  init_blockrw_read_copy_sep fmt size ;
  List.iter (init_blockrw_read_write_sep fmt size) all_sizes

let init_blockrw fmt () =
  fprintf fmt "%a@," init_blockrw_preamble () ;
  List.iter (fprintf fmt "%a" init_blockrw_read) all_sizes ;
  List.iter (fprintf fmt "%a" init_blockrw_write) all_sizes ;
  List.iter (fprintf fmt "%a" init_blockrw_all_lemmas) all_sizes

(* -------------------------------------------------------------------------- *)
(* ---  MemBytes                                                          --- *)
(* -------------------------------------------------------------------------- *)

let membytes_preamble fmt () =
  fprintf fmt "%a@,@," pp_use_list
    [ "int.Int", None
    ; "map.Map", None
    ; "frama_c_wp.cint.Cint", None
    ; "frama_c_wp.memaddr.MemAddr", None
    ; "frama_c_wp.sequence.Seq", Some "S"
    ; "RWBytes", None
    ; "ValueBlockRW", Some "VB"
    ; "InitBlockRW", Some "IB"
    ] ;
  fprintf fmt {|
  type memory = map int (VB.vblock)
  type init   = map int (IB.iblock)

  function raw_get (m: map int (map int 'a)) (a: addr) : 'a =
    get (get m a.base) a.offset

  function raw_set (m: map int (map int 'a)) (a: addr) (v: 'a) : map int (map int 'a) =
    set m a.base (set (get m a.base) a.offset v)

  let rec function to_seq (a: map int 'a) (b e: int) : S.seq 'a
    ensures { e - b >= 0 -> S.length result = e - b }
    ensures { forall i: int. b <= i < e -> S.(result[i - b]) = get a i }
    variant { e - b }
  = if e <= b then S.L.Nil else S.L.Cons (get a b) (to_seq a (b+1) e)

  function init_seq (s: int) : S.seq bool =
    S.create True s

  function memcpy
    (mtgt: map int (block 'a)) (ptgt: addr)
    (msrc: map int (block 'a)) (psrc: addr) (size: int)
    : map int (block 'a) =
    set mtgt ptgt.base (bwrite_seq (get msrc psrc.base) ptgt.offset (to_seq msrc[psrc.base] psrc.offset size))

  predicate eqmem (m1 m2: map int (block 'a)) (a: addr) (size: int) =
    beq_blocks (get m1 a.base) (get m2 a.base) (a.offset) size

|}

let membytes_write fmt t =
  fprintf fmt "@[<v 2>function write_%a (m: memory) (a: addr) (v: int) : memory =@," pp_type t;
  fprintf fmt "set m a.base (VB.bwrite_%a m [a.base] a.offset v)@,@]@," pp_type t

let membytes_read fmt t =
  fprintf fmt "@[<v 2>function read_%a (m: memory) (a: addr) : int =@," pp_type t ;
  fprintf fmt "VB.bread_%a m[a.base] a.offset@,@]@," pp_type t

let membytes_read_write_eq_same fmt t =
  let guard fmt () = fprintf fmt "is_%a v" pp_type t in
  let read_write fmt () =
    fprintf fmt "read_%a (write_%a m a v) a" pp_type t pp_type t
  in
  let eq fmt () = fprintf fmt "%a = v" read_write () in
  fprintf fmt "@[<v 2>lemma read_%a_write_%a_eq:@," pp_type t pp_type t ;
  fprintf fmt "forall m: memory, a: addr, v: int [%a].@," read_write () ;
  fprintf fmt "%a ->@,  %a@,@]@," guard () eq ()

let membytes_read_write_eq_opposite fmt rt =
  let wt = not @@ fst rt, snd rt in
  let guard fmt () = fprintf fmt "is_%a v" pp_type wt in
  let read_write fmt () =
    fprintf fmt "read_%a (write_%a m a v) a" pp_type rt pp_type wt
  in
  let eq fmt () = fprintf fmt "%a = to_%a v" read_write () pp_type rt in
  fprintf fmt "@[<v 2>lemma read_%a_write_%a_eq:@," pp_type rt pp_type wt ;
  fprintf fmt "forall m: memory, a: addr, v: int [%a].@," read_write () ;
  fprintf fmt "%a ->@,  %a@,@]@," guard () eq ()

let membytes_read_write_eq fmt t =
  membytes_read_write_eq_same fmt t ;
  membytes_read_write_eq_opposite fmt t

let membytes_read_write_sep fmt rt wt =
  let guard fmt () =
    fprintf fmt "separated aw %d ar %d" ((snd wt)/8) ((snd rt)/8) in
  let read_write fmt () =
    fprintf fmt "read_%a (write_%a m aw v) ar" pp_type rt pp_type wt
  in
  let eq fmt () = fprintf fmt "%a = read_%a m ar" read_write () pp_type rt in
  fprintf fmt "@[<v 2>lemma read_%a_write_%a_sep:@," pp_type rt pp_type wt ;
  fprintf fmt "forall m: memory, ar aw: addr, v: int [%a].@," read_write () ;
  fprintf fmt "%a ->@,  %a@,@]@," guard () eq ()

let membytes_read_copy_sep fmt rt =
  let guard fmt =
    fprintf fmt "separated ar %d aw size" ((snd rt) / 8) in
  let result fmt =
    fprintf fmt "read_%a (memcpy mw aw mc ac size) ar" pp_type rt
  in
  let eq fmt = fprintf fmt "%t = read_%a mw ar" result pp_type rt in
  fprintf fmt "@[<v 2>lemma read_%a_copy_sep:@," pp_type rt ;
  fprintf fmt "forall mw mc: memory, size: int, ar aw ac: addr [%t].@," result ;
  fprintf fmt "%t ->@,  %t@," guard eq ;
  fprintf fmt "@[<v 2>by@," ;
  fprintf fmt "let ob = mw[aw.base] in@," ;
  fprintf fmt "let nb = bwrite_seq ob aw.offset (to_seq mc[ac.base] ac.offset size) in@," ;
  fprintf fmt "aw.base = ar.base -> " ;
  fprintf fmt "VB.bread_uint%d nb ar.offset = VB.bread_uint%d ob ar.offset@]@,"
    (snd rt) (snd rt) ;
  fprintf fmt "@]@,"

let membytes_all_lemmas fmt size =
  membytes_read_write_eq fmt size ;
  membytes_read_copy_sep fmt size ;
  List.iter (membytes_read_write_sep fmt size) all_types

let membytes_write_init fmt size =
  fprintf fmt "@[<v 2>function write_init%d (m: init) (a: addr) (i: bool) : init =@," size;
  fprintf fmt "set m a.base (IB.bwrite_init%d m [a.base] a.offset i)@,@]@," size

let membytes_read_init fmt size =
  fprintf fmt "@[<v 2>function read_init%d (m: init) (a: addr) : bool =@," size ;
  fprintf fmt "IB.bread_init%d m[a.base] a.offset@,@]@," size

let membytes_read_write_init_eq fmt size =
  let read_write fmt () =
    fprintf fmt "read_init%d (write_init%d m a i) a" size size
  in
  let eq fmt () = fprintf fmt "%a = i" read_write () in
  fprintf fmt "@[<v 2>lemma read_init%d_write_init%d_eq:@," size size ;
  fprintf fmt "forall m: init, a: addr, i: bool [%a].@," read_write () ;
  fprintf fmt "%a@,@]@," eq ()

let membytes_read_write_init_sep fmt rsize wsize =
  let guard fmt () =
    fprintf fmt "separated aw %d ar %d" (wsize/8) (rsize/8) in
  let read_write fmt () =
    fprintf fmt "read_init%d (write_init%d m aw i) ar" rsize wsize
  in
  let eq fmt () = fprintf fmt "%a = read_init%d m ar" read_write () rsize in
  fprintf fmt "@[<v 2>lemma read_init%d_write_init%d_sep:@," rsize wsize ;
  fprintf fmt "forall m: init, ar aw: addr, i: bool [%a].@," read_write () ;
  fprintf fmt "%a ->@,  %a@,@]@," guard () eq ()

let membytes_read_copy_init_sep fmt rsize =
  let guard fmt =
    fprintf fmt "separated ar %d aw size" (rsize / 8) in
  let read_copy fmt =
    fprintf fmt "read_init%d (memcpy mw aw mc ac size) ar" rsize
  in
  let eq fmt = fprintf fmt "%t = read_init%d mw ar" read_copy rsize in
  fprintf fmt "@[<v 2>lemma read_init%d_copy_sep:@," rsize ;
  fprintf fmt "forall mw mc: init, size: int, ar aw ac: addr [%t].@," read_copy ;
  fprintf fmt "%t ->@,  %t@," guard eq ;
  fprintf fmt "@[<v 2>by@," ;
  fprintf fmt "let ob = mw[aw.base] in@," ;
  fprintf fmt "let nb = bwrite_seq ob aw.offset (to_seq mc[ac.base] ac.offset size) in@," ;
  fprintf fmt "aw.base = ar.base -> " ;
  fprintf fmt "IB.bread_init%d nb ar.offset = IB.bread_init%d ob ar.offset@]@,"
    rsize rsize ;
  fprintf fmt "@]@,"

let membytes_all_init_lemmas fmt size =
  membytes_read_write_init_eq fmt size ;
  membytes_read_copy_init_sep fmt size ;
  List.iter (membytes_read_write_init_sep fmt size) all_sizes

let membytes_context fmt () =
  fprintf fmt "predicate sconst (memory)@," ;
  fprintf fmt {|
  predicate bytes(m: memory) =
    forall a: addr. 0 <= raw_get m a <= 255

  predicate scinit (m: init) =
    forall a: addr. binit a.base ->
      get (get m a.base) a.offset
|}

let membytes fmt () =
  fprintf fmt "%a@," membytes_preamble () ;
  List.iter (fprintf fmt "%a" membytes_read) all_types ;
  List.iter (fprintf fmt "%a" membytes_write) all_types ;
  List.iter (fprintf fmt "%a" membytes_all_lemmas) all_types ;
  List.iter (fprintf fmt "%a" membytes_read_init) all_sizes ;
  List.iter (fprintf fmt "%a" membytes_write_init) all_sizes ;
  List.iter (fprintf fmt "%a" membytes_all_init_lemmas) all_sizes ;
  fprintf fmt "%a" membytes_context ()

(* -------------------------------------------------------------------------- *)
(* ---  Main                                                              --- *)
(* -------------------------------------------------------------------------- *)

let wmodule name builder fmt x =
  fprintf fmt "@[<v 2>module %s@," name ;
  fprintf fmt "%a" builder x ;
  fprintf fmt "@]@.end"

let file_contents filename =
  let input = open_in_bin filename in
  let contents = really_input_string input (in_channel_length input) in
  close_in input;
  contents

let create le =
  let out =
    if le
    then open_out "membytes_le.mlw"
    else open_out "membytes_be.mlw"
  in
  let header = file_contents "Wp.header" in
  let fmt = formatter_of_out_channel out in
  fprintf fmt "%s@." header ;
  fprintf fmt "(* DO NOT EDIT: this file is generated at build time *)@.@." ;
  fprintf fmt "(* Tactics for failing proofs:@.   \
               intros variables ; compute_in_goal ; cvc5 *)@.@." ;
  fprintf fmt "%a@.@;" (wmodule "ValueCodec" value_codec) () ;
  fprintf fmt "%a@.@;" (wmodule "InitCodec" init_codec) () ;
  fprintf fmt "%a@.@;" (wmodule "Offset" offset) () ;
  fprintf fmt "%a@.@;" (wmodule "RWBytes" rwbytes) le ;
  fprintf fmt "%a@.@;" (wmodule "ValueBlockRW" value_blockrw) () ;
  fprintf fmt "%a@.@;" (wmodule "InitBlockRW" init_blockrw) () ;
  fprintf fmt "%a@.@;" (wmodule "MemBytes" membytes) () ;
  close_out out

let () =
  create true ;
  create false
