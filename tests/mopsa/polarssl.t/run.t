This file has two different testing modes:
- if mopsa-build and mopsa-db are available, it tests the production of
 mopsa-db.json;
- otherwise, it uses a prepared mopsa-db.json
Building the project with make will produce lots of messages, both from Make
itself, and from GCC (e.g. warnings about sprintf)
  $ ./run-mopsa-build-if-available.sh

Test error message: does not exist
  $ frama-c -no-autoload-plugins -mopsa-db unknown-db.json
  [kernel] User Error: directory or json file 'unknown-db.json' does not exist
  [kernel] Frama-C aborted: invalid user input.
  [1]

Test error message: dir does not contain mopsa-db.json
  $ frama-c -no-autoload-plugins -mopsa-db programs
  [kernel] User Error: mopsa-db: directory 'programs' does not contain a mopsa-db.json file
  [kernel] Frama-C aborted: invalid user input.
  [1]

  $ frama-c -no-autoload-plugins -mopsa-db mopsa-db.json
  [kernel] targets:
    [library   ] library/libpolarssl.a
    [executable] programs/aes/aescrypt2
    [executable] programs/aes/crypt_and_hash
    [executable] programs/hash/generic_sum
    [executable] programs/hash/hello
    [executable] programs/hash/md5sum
    [executable] programs/hash/sha1sum
    [executable] programs/hash/sha2sum
    [executable] programs/pkey/dh_client
    [executable] programs/pkey/dh_genprime
    [executable] programs/pkey/dh_server
    [executable] programs/pkey/key_app
    [executable] programs/pkey/mpi_demo
    [executable] programs/pkey/rsa_decrypt
    [executable] programs/pkey/rsa_encrypt
    [executable] programs/pkey/rsa_genkey
    [executable] programs/pkey/rsa_sign
    [executable] programs/pkey/rsa_sign_pss
    [executable] programs/pkey/rsa_verify
    [executable] programs/pkey/rsa_verify_pss
    [executable] programs/random/gen_entropy
    [executable] programs/random/gen_random_ctr_drbg
    [executable] programs/random/gen_random_havege
    [executable] programs/ssl/ssl_client1
    [executable] programs/ssl/ssl_client2
    [executable] programs/ssl/ssl_fork_server
    [executable] programs/ssl/ssl_mail_client
    [executable] programs/ssl/ssl_server
    [executable] programs/test/benchmark
    [executable] programs/test/selftest
    [executable] programs/test/ssl_cert_test
    [executable] programs/test/ssl_test
    [executable] programs/x509/cert_app
    [executable] programs/x509/crl_app

The 'sed' below is necessary to ensure both the normalized and non-normalized
mopsa-dbs output the same paths. Otherwise, the "-I 'include'" path would
be "-I '$TESTCASE_ROOT/include'".
  $ frama-c -no-autoload-plugins -mopsa-db mopsa-db.json -mopsa-list-deps programs/ssl/ssl_client1 | sed "s|$PWD/||g"
  [kernel] dependencies:
    library/aes.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/arc4.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/asn1parse.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/base64.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/bignum.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/camellia.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/certs.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/cipher.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/cipher_wrap.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/ctr_drbg.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/debug.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/des.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/dhm.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/entropy.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/entropy_poll.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/error.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/havege.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/md.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/md2.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/md4.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/md5.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/md_wrap.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/net.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/padlock.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/pem.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/pkcs11.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/rsa.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/sha1.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/sha2.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/sha4.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/ssl_cli.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/ssl_srv.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/ssl_tls.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/timing.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/version.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/x509parse.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    library/xtea.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
    programs/ssl/ssl_client1.c:	 -I 'include' -D '_FILE_OFFSET_BITS=64'
  $ frama-c -no-autoload-plugins -load-plugin eva -mopsa-db mopsa-db.json -mopsa-target programs/ssl/ssl_client1 dummy.c
  [kernel] Parsing library/aes.c (with preprocessing)
  [kernel] Parsing library/arc4.c (with preprocessing)
  [kernel] Parsing library/asn1parse.c (with preprocessing)
  [kernel] Parsing library/base64.c (with preprocessing)
  [kernel] Parsing library/bignum.c (with preprocessing)
  [kernel] Parsing library/camellia.c (with preprocessing)
  [kernel] Parsing library/certs.c (with preprocessing)
  [kernel] Parsing library/cipher.c (with preprocessing)
  [kernel] Parsing library/cipher_wrap.c (with preprocessing)
  [kernel] Parsing library/ctr_drbg.c (with preprocessing)
  [kernel] Parsing library/debug.c (with preprocessing)
  [kernel] Parsing library/des.c (with preprocessing)
  [kernel] Parsing library/dhm.c (with preprocessing)
  [kernel] Parsing library/entropy.c (with preprocessing)
  [kernel] Parsing library/entropy_poll.c (with preprocessing)
  [kernel] Parsing library/error.c (with preprocessing)
  [kernel] Parsing library/havege.c (with preprocessing)
  [kernel] Parsing library/md.c (with preprocessing)
  [kernel] Parsing library/md2.c (with preprocessing)
  [kernel] Parsing library/md4.c (with preprocessing)
  [kernel] Parsing library/md5.c (with preprocessing)
  [kernel] Parsing library/md_wrap.c (with preprocessing)
  [kernel] Parsing library/net.c (with preprocessing)
  [kernel] Parsing library/padlock.c (with preprocessing)
  [kernel] Parsing library/pem.c (with preprocessing)
  [kernel] Parsing library/pkcs11.c (with preprocessing)
  [kernel] Parsing library/rsa.c (with preprocessing)
  [kernel] Parsing library/sha1.c (with preprocessing)
  [kernel] Parsing library/sha2.c (with preprocessing)
  [kernel] Parsing library/sha4.c (with preprocessing)
  [kernel] Parsing library/ssl_cli.c (with preprocessing)
  [kernel] Parsing library/ssl_srv.c (with preprocessing)
  [kernel] Parsing library/ssl_tls.c (with preprocessing)
  [kernel] Parsing library/timing.c (with preprocessing)
  [kernel] Parsing library/version.c (with preprocessing)
  [kernel] Parsing library/x509parse.c (with preprocessing)
  [kernel] Parsing library/xtea.c (with preprocessing)
  [kernel] Parsing programs/ssl/ssl_client1.c (with preprocessing)
  [kernel] Parsing dummy.c (with preprocessing)
  [kernel:typing:variadic] library/bignum.c:409: Warning: 
    Incorrect type for argument 3. The argument will be cast from int to unsigned int.
  [kernel:typing:variadic] library/bignum.c:2120: Warning: 
    Incorrect type for argument 2. The argument will be cast from int to unsigned int.
  [kernel:typing:variadic] library/debug.c:81: Warning: 
    Incorrect type for argument 8. The argument will be cast from int to unsigned int.
  [kernel:typing:variadic] library/debug.c:98: Warning: 
    Incorrect type for argument 7. The argument will be cast from unsigned int to int.
  [kernel:typing:variadic] library/x509parse.c:2395: Warning: 
    Incorrect type for argument 4. The argument will be cast from int to unsigned int.
  [kernel:typing:variadic] library/x509parse.c:2410: Warning: 
    Incorrect type for argument 4. The argument will be cast from int to unsigned int.
  [kernel:typing:variadic] library/x509parse.c:2462: Warning: 
    Incorrect type for argument 4. The argument will be cast from int to unsigned int.
  [kernel:typing:variadic] library/x509parse.c:2609: Warning: 
    Incorrect type for argument 4. The argument will be cast from unsigned int to int.
  [kernel:typing:variadic] library/x509parse.c:3235: Warning: 
    Incorrect type for argument 2. The argument will be cast from int to unsigned int.

# Test running Frama-C from a directory other than the one containing the
# mopsa-db.json file
  $ mkdir -p subdir && cd subdir && frama-c -no-autoload-plugins -load-plugin eva -mopsa-db ../shortened-mopsa-db.json -mopsa-target library/libpolarssl.a -kernel-warn-key typing:variadic=inactive
  [kernel] Parsing $TESTCASE_ROOT/library/aes.c (with preprocessing)
  [kernel] Parsing $TESTCASE_ROOT/library/arc4.c (with preprocessing)
