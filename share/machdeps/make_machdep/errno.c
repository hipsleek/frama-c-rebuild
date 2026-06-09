/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include <errno.h>

/* Mandatory */
#ifdef EDOM
int edom_is = EDOM;
#endif
#ifdef EILSEQ
int eilseq_is = EILSEQ;
#endif
#ifdef ERANGE
int erange_is = ERANGE;
#endif

/* Implementation defined by POSIX and GNU Linux */
#ifdef E2BIG
int e2big_is = E2BIG;
#endif
#ifdef EACCES
int eacces_is = EACCES;
#endif
#ifdef EADDRINUSE
int eaddrinuse_is = EADDRINUSE;
#endif
#ifdef EADDRNOTAVAIL
int eaddrnotavail_is = EADDRNOTAVAIL;
#endif
#ifdef EAFNOSUPPORT
int eafnosupport_is = EAFNOSUPPORT;
#endif
#ifdef EAGAIN
int eagain_is = EAGAIN;
#endif
#ifdef EALREADY
int ealready_is = EALREADY;
#endif
#ifdef EBADE
int ebade_is = EBADE;
#endif
#ifdef EBADF
int ebadf_is = EBADF;
#endif
#ifdef EBADFD
int ebadfd_is = EBADFD;
#endif
#ifdef EBADMSG
int ebadmsg_is = EBADMSG;
#endif
#ifdef EBADR
int ebadr_is = EBADR;
#endif
#ifdef EBADRQC
int ebadrqc_is = EBADRQC;
#endif
#ifdef EBADSLT
int ebadslt_is = EBADSLT;
#endif
#ifdef EBUSY
int ebusy_is = EBUSY;
#endif
#ifdef ECANCELED
int ecanceled_is = ECANCELED;
#endif
#ifdef ECHILD
int echild_is = ECHILD;
#endif
#ifdef ECHRNG
int echrng_is = ECHRNG;
#endif
#ifdef ECOMM
int ecomm_is = ECOMM;
#endif
#ifdef ECONNABORTED
int econnaborted_is = ECONNABORTED;
#endif
#ifdef ECONNREFUSED
int econnrefused_is = ECONNREFUSED;
#endif
#ifdef ECONNRESET
int econnreset_is = ECONNRESET;
#endif
#ifdef EDEADLK
int edeadlk_is = EDEADLK;
#endif
#ifdef EDEADLOCK
int edeadlock_is = EDEADLOCK;
#endif
#ifdef EDESTADDRREQ
int edestaddrreq_is = EDESTADDRREQ;
#endif
#ifdef EDQUOT
int edquot_is = EDQUOT;
#endif
#ifdef EEXIST
int eexist_is = EEXIST;
#endif
#ifdef EFAULT
int efault_is = EFAULT;
#endif
#ifdef EFBIG
int efbig_is = EFBIG;
#endif
#ifdef EHOSTDOWN
int ehostdown_is = EHOSTDOWN;
#endif
#ifdef EHOSTUNREACH
int ehostunreach_is = EHOSTUNREACH;
#endif
#ifdef EIDRM
int eidrm_is = EIDRM;
#endif
#ifdef EINPROGRESS
int einprogress_is = EINPROGRESS;
#endif
#ifdef EINTR
int eintr_is = EINTR;
#endif
#ifdef EINVAL
int einval_is = EINVAL;
#endif
#ifdef EIO
int eio_is = EIO;
#endif
#ifdef EISCONN
int eisconn_is = EISCONN;
#endif
#ifdef EISDIR
int eisdir_is = EISDIR;
#endif
#ifdef EISNAM
int eisnam_is = EISNAM;
#endif
#ifdef EKEYEXPIRED
int ekeyexpired_is = EKEYEXPIRED;
#endif
#ifdef EKEYREJECTED
int ekeyrejected_is = EKEYREJECTED;
#endif
#ifdef EKEYREVOKED
int ekeyrevoked_is = EKEYREVOKED;
#endif
#ifdef EL2HLT
int el2hlt_is = EL2HLT;
#endif
#ifdef EL2NSYNC
int el2nsync_is = EL2NSYNC;
#endif
#ifdef EL3HLT
int el3hlt_is = EL3HLT;
#endif
#ifdef EL3RST
int el3rst_is = EL3RST;
#endif
#ifdef ELIBACC
int elibacc_is = ELIBACC;
#endif
#ifdef ELIBBAD
int elibbad_is = ELIBBAD;
#endif
#ifdef ELIBMAX
int elibmax_is = ELIBMAX;
#endif
#ifdef ELIBSCN
int elibscn_is = ELIBSCN;
#endif
#ifdef ELIBEXEC
int elibexec_is = ELIBEXEC;
#endif
#ifdef ELOOP
int eloop_is = ELOOP;
#endif
#ifdef EMEDIUMTYPE
int emediumtype_is = EMEDIUMTYPE;
#endif
#ifdef EMFILE
int emfile_is = EMFILE;
#endif
#ifdef EMLINK
int emlink_is = EMLINK;
#endif
#ifdef EMSGSIZE
int emsgsize_is = EMSGSIZE;
#endif
#ifdef EMULTIHOP
int emultihop_is = EMULTIHOP;
#endif
#ifdef ENAMETOOLONG
int enametoolong_is = ENAMETOOLONG;
#endif
#ifdef ENETDOWN
int enetdown_is = ENETDOWN;
#endif
#ifdef ENETRESET
int enetreset_is = ENETRESET;
#endif
#ifdef ENETUNREACH
int enetunreach_is = ENETUNREACH;
#endif
#ifdef ENFILE
int enfile_is = ENFILE;
#endif
#ifdef ENOBUFS
int enobufs_is = ENOBUFS;
#endif
#ifdef ENODATA
int enodata_is = ENODATA;
#endif
#ifdef ENODEV
int enodev_is = ENODEV;
#endif
#ifdef ENOENT
int enoent_is = ENOENT;
#endif
#ifdef ENOEXEC
int enoexec_is = ENOEXEC;
#endif
#ifdef ENOKEY
int enokey_is = ENOKEY;
#endif
#ifdef ENOLCK
int enolck_is = ENOLCK;
#endif
#ifdef ENOLINK
int enolink_is = ENOLINK;
#endif
#ifdef ENOMEDIUM
int enomedium_is = ENOMEDIUM;
#endif
#ifdef ENOMEM
int enomem_is = ENOMEM;
#endif
#ifdef ENOMSG
int enomsg_is = ENOMSG;
#endif
#ifdef ENONET
int enonet_is = ENONET;
#endif
#ifdef ENOPKG
int enopkg_is = ENOPKG;
#endif
#ifdef ENOPROTOOPT
int enoprotoopt_is = ENOPROTOOPT;
#endif
#ifdef ENOSPC
int enospc_is = ENOSPC;
#endif
#ifdef ENOSR
int enosr_is = ENOSR;
#endif
#ifdef ENOSTR
int enostr_is = ENOSTR;
#endif
#ifdef ENOSYS
int enosys_is = ENOSYS;
#endif
#ifdef ENOTBLK
int enotblk_is = ENOTBLK;
#endif
#ifdef ENOTCONN
int enotconn_is = ENOTCONN;
#endif
#ifdef ENOTDIR
int enotdir_is = ENOTDIR;
#endif
#ifdef ENOTEMPTY
int enotempty_is = ENOTEMPTY;
#endif
#ifdef ENOTRECOVERABLE
int enotrecoverable_is = ENOTRECOVERABLE;
#endif
#ifdef ENOTSOCK
int enotsock_is = ENOTSOCK;
#endif
#ifdef ENOTSUP
int enotsup_is = ENOTSUP;
#endif
#ifdef ENOTTY
int enotty_is = ENOTTY;
#endif
#ifdef ENOTUNIQ
int enotuniq_is = ENOTUNIQ;
#endif
#ifdef ENXIO
int enxio_is = ENXIO;
#endif
#ifdef EOPNOTSUPP
int eopnotsupp_is = EOPNOTSUPP;
#endif
#ifdef EOVERFLOW
int eoverflow_is = EOVERFLOW;
#endif
#ifdef EOWNERDEAD
int eownerdead_is = EOWNERDEAD;
#endif
#ifdef EPERM
int eperm_is = EPERM;
#endif
#ifdef EPFNOSUPPORT
int epfnosupport_is = EPFNOSUPPORT;
#endif
#ifdef EPIPE
int epipe_is = EPIPE;
#endif
#ifdef EPROTO
int eproto_is = EPROTO;
#endif
#ifdef EPROTONOSUPPORT
int eprotonosupport_is = EPROTONOSUPPORT;
#endif
#ifdef EPROTOTYPE
int eprototype_is = EPROTOTYPE;
#endif
#ifdef EREMCHG
int eremchg_is = EREMCHG;
#endif
#ifdef EREMOTE
int eremote_is = EREMOTE;
#endif
#ifdef EREMOTEIO
int eremoteio_is = EREMOTEIO;
#endif
#ifdef ERESTART
int erestart_is = ERESTART;
#endif
#ifdef EROFS
int erofs_is = EROFS;
#endif
#ifdef ESHUTDOWN
int eshutdown_is = ESHUTDOWN;
#endif
#ifdef ESPIPE
int espipe_is = ESPIPE;
#endif
#ifdef ESOCKTNOSUPPORT
int esocktnosupport_is = ESOCKTNOSUPPORT;
#endif
#ifdef ESRCH
int esrch_is = ESRCH;
#endif
#ifdef ESTALE
int estale_is = ESTALE;
#endif
#ifdef ESTRPIPE
int estrpipe_is = ESTRPIPE;
#endif
#ifdef ETIME
int etime_is = ETIME;
#endif
#ifdef ETIMEDOUT
int etimedout_is = ETIMEDOUT;
#endif
#ifdef ETXTBSY
int etxtbsy_is = ETXTBSY;
#endif
#ifdef EUCLEAN
int euclean_is = EUCLEAN;
#endif
#ifdef EUNATCH
int eunatch_is = EUNATCH;
#endif
#ifdef EUSERS
int eusers_is = EUSERS;
#endif
#ifdef EWOULDBLOCK
int ewouldblock_is = EWOULDBLOCK;
#endif
#ifdef EXDEV
int exdev_is = EXDEV;
#endif
#ifdef EXFULL
int exfull_is = EXFULL;
#endif
