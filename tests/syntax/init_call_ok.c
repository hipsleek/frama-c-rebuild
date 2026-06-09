/* run.config
   EXIT: 0
   STDOPT:
*/
int h(void);

char g[];
char* z(void);

int main(void) {
    char c[] = {0,1};
    char d[] = {h()};
    char e[] = "abcd";
    static char l[2];
    static char i[] = {0,1};
    extern char k[];
    struct {
        int A[sizeof(char[8])];
        int i;
    } A = { h(), 2, 3, 4, 5, 6, 7, 8, 9 };
    return 0;
}
