/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_RESOLV_H
#define __FC_RESOLV_H
#include "features.h"
#include <stdint.h> // for uint32_t
#include "__fc_inet.h" // for struct sockaddr_in

__PUSH_FC_STDLIB

__BEGIN_DECLS

// Note: resolv.h is neither ISO-C nor POSIX

#define MAXNS 3
#define MAXDFLSRCH 3
#define MAXDNSRCH 6
#define MAXRESOLVSORT 10

struct __res_state {
  int retrans;
  int retry;
  unsigned long options;
  int nscount;
  struct sockaddr_in nsaddr_list[MAXNS];
  unsigned short id;
  char *dnsrch[MAXDNSRCH+1];
  char defdname[256];
  unsigned long pfcode;
  unsigned ndots:4;
  unsigned nsort:4;
  unsigned ipv6_unavail:1;
  unsigned unused:23;
  struct __res_state_sort_list {
    struct in_addr addr;
    uint32_t mask;
  } sort_list[MAXRESOLVSORT];
  char* _unused_qhook;
  char* _unused_rhook;
  int res_h_errno;
  int _vcsock;
  unsigned int _flags;
  struct __res_state__ext {
    uint16_t nscount;
    uint16_t nsmap[MAXNS];
    int nssocks[MAXNS];
    uint16_t nscount6;
    uint16_t nsinit;
    struct sockaddr_in6 *nsaddrs[MAXNS];
    unsigned int __glibc_reserved[2];
  } _ext; // Note: in the GNU libc, this field is inside a union, but this
          // would case some initialization issues with Eva.
          // The union has been removed, but if needed to parse some code,
          // it might be added back.
};

#define RES_INIT 0x00000001
#define RES_DEBUG 0x00000002
#define RES_AAONLY 0x00000004
#define RES_USEVC 0x00000008
#define RES_PRIMARY 0x00000010
#define RES_IGNTC 0x00000020
#define RES_RECURSE 0x00000040
#define RES_DEFNAMES 0x00000080
#define RES_STAYOPEN 0x00000100
#define RES_DNSRCH 0x00000200
#define RES_NOALIASES 0x00001000
#define RES_ROTATE 0x00004000
#define RES_NOCHECKNAME 0x00008000
#define RES_KEEPTSIG 0x00010000
#define RES_BLAST 0x00020000
#define RES_USE_EDNS0 0x00100000
#define RES_SNGLKUP 0x00200000
#define RES_SNGLKUPREOP 0x00400000
#define RES_USE_DNSSEC 0x00800000
#define RES_NOTLDQUERY 0x01000000
#define RES_NORELOAD 0x02000000
#define RES_TRUSTAD 0x04000000
#define RES_NOAAAA 0x08000000
#define RES_STRICTERR 0x10000000

#define RES_DEFAULT (RES_RECURSE|RES_DEFNAMES|RES_DNSRCH)

#define RES_PRF_STATS 0x00000001
#define RES_PRF_UPDATE 0x00000002
#define RES_PRF_CLASS 0x00000004
#define RES_PRF_CMD 0x00000008
#define RES_PRF_QUES 0x00000010
#define RES_PRF_ANS 0x00000020
#define RES_PRF_AUTH 0x00000040
#define RES_PRF_ADD 0x00000080
#define RES_PRF_HEAD1 0x00000100
#define RES_PRF_HEAD2 0x00000200
#define RES_PRF_TTLID 0x00000400
#define RES_PRF_HEADX 0x00000800
#define RES_PRF_QUERY 0x00001000
#define RES_PRF_REPLY 0x00002000
#define RES_PRF_INIT 0x00004000


typedef struct __res_state *res_state;

volatile struct __res_state __fc_resolv; // internal state;

/*@
  assigns \result \from &__fc_resolv;
*/
extern struct __res_state *__res_state(void) __attribute__ ((__const__));
#define _res (*__res_state())

// deprecated
/*@
  assigns \result \from indirect:__fc_resolv;
  assigns __fc_resolv \from __fc_resolv;
*/
extern int res_init(void);

/*@
  assigns *statep \from *statep;
  assigns \result \from indirect:*statep;
*/
extern int res_ninit(res_state statep);

/*@
  assigns \result, *statep, answer[0 .. anslen - 1] \from *statep,
    indirect:dname[0..], indirect:class, indirect:type, indirect:anslen;
*/
extern int res_nquery(res_state statep,
                      const char *dname, int class, int type,
                      unsigned char *answer, int anslen);

/*@
  assigns \result, *statep, answer[0 .. anslen - 1] \from *statep,
    indirect:dname[0..], indirect:class, indirect:type, indirect:anslen;
*/
extern int res_nsearch(res_state statep,
                       const char *dname, int class, int type,
                       unsigned char *answer, int anslen);

/*@
  assigns \result, *statep, answer[0 .. anslen - 1] \from *statep,
    indirect:name[0..], indirect:domain[0..], indirect:class, indirect:type,
    indirect:anslen;
*/
extern int res_nquerydomain(res_state statep,
                            const char *name, const char *domain,
                            int class, int type, unsigned char *answer,
                            int anslen);

/*@
  assigns \result, __fc_resolv, buf[0 .. buflen - 1] \from __fc_resolv,
    indirect:op, indirect:dname[0..], indirect:class, indirect:datalen,
    indirect:newrr[0..], indirect:buflen;
*/
extern int res_mkquery(int op, const char *dname, int class,
                       int type, const unsigned char *data, int datalen,
                       const unsigned char *newrr,
                       unsigned char *buf, int buflen);

/*@
  assigns \result, *statep, buf[0 .. buflen - 1] \from *statep,
    indirect:op, indirect:dname[0..], indirect:class, indirect:datalen,
    indirect:newrr[0..], indirect:buflen;
*/
extern int res_nmkquery(res_state statep,
                        int op, const char *dname, int class,
                        int type, const unsigned char *data, int datalen,
                        const unsigned char *newrr,
                        unsigned char *buf, int buflen);

/*@
  assigns \result, __fc_resolv, answer[0 .. anslen - 1] \from __fc_resolv,
    msg[0 .. msglen - 1], indirect:anslen;
*/
extern int res_send(const unsigned char *msg, int msglen,
                    unsigned char *answer, int anslen);
/*@
  assigns \result, *statep, answer[0 .. anslen - 1] \from *statep,
    msg[0 .. msglen - 1], indirect:anslen;
*/
extern int res_nsend(res_state statep,
                     const unsigned char *msg, int msglen,
                     unsigned char *answer, int anslen);

/*@
  //missing: assigns \from 'resolver'
  assigns \result \from indirect:exp_dn[0..], indirect:dnptrs[0..][0..],
                        indirect:comp_dn[0..];
  assigns comp_dn[0 .. length-1] \from exp_dn[0..], dnptrs[0..][0..],
                                       indirect:(*lastdnptr)[0..];
*/
extern int dn_comp(const char *exp_dn, unsigned char *comp_dn,
                   int length, unsigned char **dnptrs,
                   unsigned char **lastdnptr);

/*@
  //missing: assigns \from 'resolver'
  assigns \result \from indirect:msg[0..], indirect:eomorig[0..],
                        indirect:comp_dn[0..];
  assigns exp_dn[0 .. length-1] \from msg[0..], eomorig[0..], comp_dn[0..];
*/
extern int dn_expand(const unsigned char *msg,
                     const unsigned char *eomorig,
                     const unsigned char *comp_dn, char *exp_dn,
                     int length);

/*@
  assigns \result \from indirect:comp_dn[0 ..], indirect:eom;
*/
int dn_skipname(const unsigned char *comp_dn, const unsigned char *eom);

__END_DECLS

__POP_FC_STDLIB
#endif
