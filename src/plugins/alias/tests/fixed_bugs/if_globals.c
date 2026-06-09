/* run.config
   COMMENT: Check that graphs from both branches are correctly merged
   STDOPT: #"-alias-debug 3"
*/

int *p;
int z;
int d;

void main() {
    if (1-1) {
        p = &z;
    }
    else
    {
        p = &d;
    }
}
