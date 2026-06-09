/* run.config
   COMMENT: Formal Verification of PKCS#1 Signature Parser using Frama-C
   COMMENT: https://doi.org/10.1007/978-3-032-10794-7_17
   COMMENT: artificially rendered translatable: bound prev_pars, nest_prev_pars
   STDOPT: +"-eva-unroll-recursive-calls 5"
*/

#include "limits.h"
typedef unsigned char u1;
typedef unsigned int uint;

// Parsing errors                // Unexpected:
#define ERR_B1              0x01 //First prefix byte
#define ERR_B2              0x02 //Second prefix byte
#define ERR_FF_COUNT        0x03 //Less than minimum of pad bytes
#define ERR_DELIM           0x04 //Delimiter between pad and ASN
#define ERR_TAG             0x05 //TLV Tag
#define ERR_LEN             0x06 //TLV Length
#define ERR_HASH_SZ         0x08 //Hash length
#define ERR_TLV_LEN_CONSIST 0x09 //Nesting TLV property
#define ERR_HASH_OID        0x0A //Hash identifier
#define ERR_NULL_SZ         0x0B //Hash parameter size
#define ERR_HASH_VAL        0x0C //Hash value
//Hash crypto call error
#define ERR_INTER_BAD_CRYPT 0xF2
// Nominal parsing case
#define PASS_OK 0
// Parsing offsets and lengths
#define OFF_B1     0 // Prefix padding byte 1 offset
#define OFF_B2     1 // Prefix padding byte 2 offset
#define LEN_TAGLEN 2 // Total length of 2 fields: TAG + LEN
// Expected values during parsing
#define EXP_B1       0x00 // Prefix padding byte 1
#define EXP_B2       0x01 // Prefix padding byte 2
#define EXP_FF       0xFF // Padding 0xFF sequence
#define EXP_DELIM    0x00 // Padding delimiter
#define MIN_FF_COUNT 0x08 // Minimal size of FF sequence
#define TAG_SEQ      0x30 // ASN.1 SEQUENCE Tag
#define TAG_OCT      0x04 // ASN.1 OCTET_STRING Tag
#define TAG_OID      0x06 // ASN.1 OID Tag
#define TAG_NULL     0x05 // ASN.1 NULL Tag
#define ASN_LEN_NULL 0x00 // ASN.1 NULL Length
// Supported hash functions
#define SUPP_OID_COUNT 3
#define MAX_HASH_SIZE  64
#define SHA256_SIZE    32
#define SHA512_SIZE    64
#define SHA1_SIZE      20

enum hash_functions { SHA256_INDEX, SHA512_INDEX, SHA1_INDEX };

// Element 0 contains size of OID, followed by its value
const u1 SHA256_OID[10] = {0x09, 0x60, 0x86, 0x48, 0x01,
                           0x65, 0x03, 0x04, 0x02, 0x01};
const u1 SHA512_OID[10] = {0x09, 0x60, 0x86, 0x48, 0x01,
                           0x65, 0x03, 0x04, 0x02, 0x03};
const u1 SHA1_OID[7] = {0x05, 0x2B, 0x0E, 0x03, 0x02, 0x1A};

const u1 *const OID_list[SUPP_OID_COUNT] = {
    (u1 *)&SHA256_OID[0], (u1 *)&SHA512_OID[0], (u1 *)&SHA1_OID[0]};

u1 hash_res[MAX_HASH_SIZE]; // Working buffer for hash computation

const int hash_len_for_oid[SUPP_OID_COUNT] = {SHA256_SIZE, SHA512_SIZE,
                                              SHA1_SIZE};

#define OFF_COUNT    0
#define SPEC_ITEM_SZ 3
#define OFF_TAG      1
#define OFF_TYPE     2
#define OFF_LINK     3
#define PRIM         1
#define CONSTR       0

// ======= TLV structure specification =======
/*@ ghost
const uint g_tlv_1[4] = {1, TAG_SEQ, CONSTR, 1};
const uint g_tlv_2[7] = {2, TAG_SEQ, CONSTR, 2, TAG_OCT,  PRIM, 0};
const uint g_tlv_3[7] = {2, TAG_OID, PRIM,   0, TAG_NULL, PRIM, 0};

\ghost const uint* const g_tlv_spec[3] =
  { &g_tlv_1[0], &g_tlv_2[0], &g_tlv_3[0] };

u1* g_tlv_1_p[1]; u1* g_tlv_2_p[2]; u1* g_tlv_3_p[2];

u1* \ghost * const g_tlv_p[3] =
  { &g_tlv_1_p[0], &g_tlv_2_p[0], &g_tlv_3_p[0] };  */

// Correct structure of message with respect to TLV specification g_tlv_spec
/*@
// ======= Inductive predicate for correct message format =======
inductive valid_tlv(u1* start, integer prev_pars, integer size_pars,
  integer tlv_num_pars, integer tlv_id, boolean with_children)
{
case empty: \forall u1* start, integer tlv_id;
  valid_tlv(start, 0, 0, 0, tlv_id, \true);

case new_tlv_child_not_incl:
  \forall u1* start, integer prev_pars, integer size_pars,
    integer tlv_num_pars, integer tlv_id;
  \let tlv_cur = g_tlv_spec[tlv_id];

  // MODIFICATION REQUIRED FOR TRANSLATION TO WORK
  \let prev_pars = 0;

  0 <= tlv_num_pars < tlv_cur[OFF_COUNT] &&
  valid_tlv(start, prev_pars, size_pars, tlv_num_pars, tlv_id, \true) &&
  start[size_pars] == (u1)tlv_cur[OFF_TAG + tlv_num_pars*SPEC_ITEM_SZ]
  ==>
  valid_tlv(start, size_pars, size_pars + 2 + start[size_pars + 1],
    tlv_num_pars+1, tlv_id, \false);

case last_tlv_prim_incl_child:
  \forall u1* start, integer prev_pars, integer size_pars,
    integer tlv_num_pars, integer tlv_id;
  \let tlv_cur = g_tlv_spec[tlv_id];
  1 <= tlv_num_pars &&
  valid_tlv(start, prev_pars, size_pars, tlv_num_pars, tlv_id, \false) &&
  (u1)tlv_cur[OFF_TYPE + (tlv_num_pars-1)*SPEC_ITEM_SZ] == PRIM
  ==>
  valid_tlv(start, prev_pars, size_pars, tlv_num_pars, tlv_id, \true);

case last_tlv_constr_incl_child:
  \forall u1* start, integer prev_pars, integer size_pars,
    integer tlv_num_pars, integer tlv_id, integer nest_prev_pars;
  \let tlv_cur = g_tlv_spec[tlv_id];
  \let tlv_link = tlv_cur[OFF_LINK + (tlv_num_pars-1)*SPEC_ITEM_SZ];

  // MODIFICATION REQUIRED FOR TRANSLATION TO WORK
  \let nest_prev_pars = 0;

  1 <= tlv_num_pars &&
  valid_tlv(start, prev_pars, size_pars, tlv_num_pars, tlv_id, \false) &&
  (u1)tlv_cur[OFF_TYPE + (tlv_num_pars-1)*SPEC_ITEM_SZ] == CONSTR &&
  valid_tlv(start + prev_pars + 2, nest_prev_pars, start[prev_pars + 1],
    g_tlv_spec[tlv_link][OFF_COUNT], tlv_link, \true)
  ==>
  valid_tlv(start, prev_pars, size_pars, tlv_num_pars, tlv_id, \true); }

// Supplementary predicate
// To ease instantiation of prim_gh_set manually asserted in the code
predicate prim_tlv_stored(u1* start, integer tlv_id, integer tlv_seq_id,
    integer prev_pars) =
  \let tlv_cur = g_tlv_spec[tlv_seq_id];
  \let tlv_p_curr = g_tlv_p[tlv_seq_id];
  (u1)tlv_cur[OFF_TYPE + (tlv_id-1)*SPEC_ITEM_SZ] == PRIM &&
  tlv_p_curr[tlv_id-1] == &start[prev_pars];

// Ghost code correctly store pointers to all primitive TLVs
inductive prim_gh_set (u1* start, integer prev_pars, integer size_pars,
  integer tlv_num_pars, integer tlv_id, boolean with_children)
{
case gh_empty: \forall u1* start, integer tlv_id;
  prim_gh_set (start, 0, 0, 0, tlv_id, \true);

case gh_new_tlv_child_not_incl:
  \forall u1* start, integer prev_pars, integer size_pars,
    integer tlv_num_pars, integer tlv_id;
  \let tlv_cur = g_tlv_spec[tlv_id];

  // MODIFICATION REQUIRED FOR TRANSLATION TO WORK
  \let prev_pars = 0;

  0 <= tlv_num_pars < tlv_cur[OFF_COUNT] &&
  prim_gh_set(start, prev_pars, size_pars, tlv_num_pars, tlv_id, \true)
  ==>
  prim_gh_set(start, size_pars, size_pars + 2 + start[size_pars + 1],
    tlv_num_pars+1, tlv_id, \false);

case gh_last_tlv_prim_incl_child:
  \forall u1* start, integer prev_pars, integer size_pars,
    integer tlv_num_pars, integer tlv_id;
  \let tlv_cur = g_tlv_spec[tlv_id];
  1 <= tlv_num_pars &&
  prim_gh_set(start, prev_pars, size_pars, tlv_num_pars, tlv_id, \false) &&
  prim_tlv_stored(start, tlv_num_pars, tlv_id, prev_pars)
  ==>
  prim_gh_set(start, prev_pars, size_pars, tlv_num_pars, tlv_id, \true);

case gh_last_tlv_constr_incl_child:
  \forall u1* start, integer prev_pars, integer size_pars,
    integer tlv_num_pars, integer tlv_id, integer nest_prev_pars;
  \let tlv_cur = g_tlv_spec[tlv_id];
  \let tlv_link = tlv_cur[OFF_LINK + (tlv_num_pars-1)*SPEC_ITEM_SZ];

  // MODIFICATION REQUIRED FOR TRANSLATION TO WORK
  \let nest_prev_pars = 0;

  1 <= tlv_num_pars &&
  prim_gh_set(start, prev_pars, size_pars, tlv_num_pars, tlv_id, \false) &&
  (u1)tlv_cur[OFF_TYPE + (tlv_num_pars-1)*SPEC_ITEM_SZ] == CONSTR &&
  prim_gh_set(start + prev_pars + 2, nest_prev_pars, start[prev_pars + 1],
    g_tlv_spec[tlv_link][OFF_COUNT], tlv_link, \true)
  ==>
  prim_gh_set(start, prev_pars, size_pars, tlv_num_pars, tlv_id, \true);
}*/

u1 u1val = 0;
u1 *u1ptr = &u1val;

int main() {
  //@ assert valid_tlv(u1ptr, 0, 0, 0, 0, \true);
  //@ assert prim_gh_set (u1ptr, 0, 0, 0, 0, \true);
  return 0;
}
