/* _Nullable is a macOS-specific qualifier. For now, we just ignore it. */

int * _Nullable test(void) { return (int*)0; }

extern int (* _Nullable _fptr)(void);

int f(int * _Nullable x) { if (x) return *x; else return 0; }
