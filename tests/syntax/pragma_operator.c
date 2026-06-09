// The pragma below is allowed by GCC (14), but only on a .c file.
// Clang (18) also accepts it on a .i file.
int _Pragma("entrypoint") main( void) {
    return 0;
}

void _Pragma("entrypoint") main2();

void _Pragma("entrypoint") main2() {
}

void _Pragma("entrypoint") _Pragma("another pragma") _Pragma("") main3() {
}
