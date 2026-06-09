/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_STROPTS_H
#define __FC_STROPTS_H
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

#include "__fc_define_uid_and_gid.h"
#include "__fc_define_ioctl.h"
#include "__fc_machdep.h"

typedef __INT32_T t_scalar_t;
typedef __UINT32_T t_uscalar_t;

struct bandinfo {
  int            bi_flag;
  unsigned char  bi_pri;
};

struct strbuf {
  char  *buf;
  int    len;
  int    maxlen;
};

struct strpeek {
  struct strbuf  ctlbuf;
  struct strbuf  databuf;
  t_uscalar_t    flags;
};

struct strfdinsert {
  struct strbuf  ctlbuf;
  struct strbuf  databuf;
  int            fildes;
  t_uscalar_t    flags;
  int            offset;
};

struct strioctl {
  int    ic_cmd;
  char  *ic_dp;
  int    ic_len;
  int    ic_timout;
};

struct strrecvfd {
  int    fd;
  gid_t  gid;
  uid_t  uid;
};

#define FMNAMESZ 8

struct str_mlist {
  char  l_name[FMNAMESZ+1];
};

struct str_list {
  struct str_mlist  *sl_modlist;
  int                sl_nmods;
};

// The values for the constants below are based on those of the glibc,
// declared in the order given by POSIX.1-2008.

#define I_ATMARK (__SID |31)
#define I_CANPUT (__SID |34)
#define I_CKBAND (__SID |29)
#define I_FDINSERT (__SID |16)
#define I_FIND (__SID |11)
#define I_FLUSH (__SID | 5)
#define I_FLUSHBAND (__SID |28)
#define I_FLUSHBAND (__SID |28)
#define I_GETBAND (__SID |30)
#define I_GETCLTIME (__SID |33)
#define I_GETSIG (__SID |10)
#define I_GRDOPT (__SID | 7)
#define I_GWROPT (__SID |20)
#define I_LINK (__SID |12)
#define I_LIST (__SID |21)
#define I_LOOK (__SID | 4)
#define I_NREAD (__SID | 1)
#define I_PEEK (__SID |15)
#define I_PLINK (__SID |22)
#define I_POP (__SID | 3)
#define I_PUNLINK (__SID |23)
#define I_PUSH (__SID | 2)
#define I_RECVFD (__SID |14)
#define I_SENDFD (__SID |17)
#define I_SETCLTIME (__SID |32)
#define I_SETSIG (__SID | 9)
#define I_SRDOPT (__SID | 6)
#define I_STR (__SID | 8)
#define I_SWROPT (__SID |19)
#define I_UNLINK (__SID |13)

#define FLUSHR 0x01
#define FLUSHRW 0x03
#define FLUSHRW 0x03
#define FLUSHW 0x02

#define S_BANDURG 0x0200
#define S_ERROR 0x0010
#define S_HANGUP 0x0020
#define S_HIPRI 0x0002
#define S_INPUT 0x0001
#define S_MSG 0x0008
#define S_OUTPUT 0x0004
#define S_RDBAND 0x0080
#define S_RDNORM 0x0040
#define S_WRBAND 0x0100
#define S_WRNORM S_OUTPUT

#define RS_HIPRI 0x01

#define RMSGD 0x0001
#define RMSGN 0x0002
#define RNORM 0x0000
#define RPROTDAT 0x0004
#define RPROTDIS 0x0008
#define RPROTNORM 0x0010

#define SNDZERO 0x001

#define ANYMARK 0x01
#define LASTMARK 0x02

#define MUXID_ALL (-1)

#define MORECTL 1
#define MOREDATA 2
#define MSG_ANY 0x02
#define MSG_BAND 0x04
#define MSG_HIPRI 0x01

/*@
  assigns \result \from indirect:fildes, indirect:path[0..];
    //missing: assigns 'filesystem' \from 'filesystem'
*/
extern int fattach(int fildes, const char *path);

/*@
  assigns \result \from indirect:path[0..];
    //missing: assigns 'filesystem' \from 'filesystem'
*/
extern int fdetach(const char *path);

/*@
  //missing, for all clauses below: \from 'STREAMS-based file'
  assigns \result \from indirect:fildes, indirect:ctlptr->maxlen,
                        indirect:dataptr->maxlen, indirect:*flagsp;
  assigns *ctlptr \from indirect:fildes, *ctlptr, indirect:*flagsp;
  assigns *dataptr \from indirect:fildes, *dataptr, indirect:*flagsp;
  assigns *flagsp \from indirect:fildes, *flagsp;
*/
extern int getmsg(int fildes, struct strbuf *restrict ctlptr,
                  struct strbuf *restrict dataptr, int *restrict flagsp);

/*@
  //missing, for all clauses below: \from 'STREAMS-based file'
  assigns \result \from indirect:fildes, indirect:ctlptr->maxlen,
                        indirect:dataptr->maxlen, indirect:*flagsp;
  assigns *ctlptr \from indirect:fildes, *ctlptr, indirect:*flagsp;
  assigns *dataptr \from indirect:fildes, *dataptr, indirect:*flagsp;
  assigns *flagsp \from indirect:fildes, *flagsp;
*/
extern int getpmsg(int fildes, struct strbuf *restrict ctlptr,
                   struct strbuf *restrict dataptr, int *restrict bandp,
                   int *restrict flagsp);

/*@
  assigns \result \from indirect:fildes; //missing: \from 'STREAMS-based file'
*/
extern int isastream(int fildes);

/*@
  //missing, for all clauses below: assigns + \from 'STREAMS-based file'
  assigns \result \from indirect:fildes, indirect:*ctlptr, indirect:*dataptr,
                        indirect:flags;
*/
extern int putmsg(int fildes, const struct strbuf *ctlptr,
           const struct strbuf *dataptr, int flags);

/*@
  //missing, for all clauses below: assigns + \from 'STREAMS-based file'
  assigns \result \from indirect:fildes, indirect:*ctlptr, indirect:*dataptr,
                        indirect:band, indirect:flags;
*/
extern int putpmsg(int fildes, const struct strbuf *ctlptr,
            const struct strbuf *dataptr, int band, int flags);

__END_DECLS
__POP_FC_STDLIB
#endif
