We have to redirect cmake's output because it contains several non-deterministic
messages, such as '-- Configuring done (0.1s)'
  $ mkdir build
  $ cd build
  $ mopsa-build cmake .. >cmake.out 2>cmake.err
  $ rm mopsa.db # remove CMake test files, e.g. CMakeFiles/CMakeScratch/TryCompile...
  $ mopsa-build make >make2.out 2>make2.err
  $ mopsa-db -json > mopsa-db.json
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
  $ cd ..
  $ frama-c -no-autoload-plugins -mopsa-db build -mopsa-target library/libpolarssl.a
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
