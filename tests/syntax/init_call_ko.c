/* run.config
   EXIT: 1
   STDOPT:
*/
char *b(char* c){ return c; }

int main(void) {
    char a[] = b(""); // cannot be initialized with a function call
    char m[] = 1; // cannot be initialized with a constant
    char p[1] = 0; // even if sized
    extern char j[] = {0,1}; // extern local variable cannot be initialized
    char f[]; // unsized array must be initialized
    static char n[]; // even if static
    return 0;
}
