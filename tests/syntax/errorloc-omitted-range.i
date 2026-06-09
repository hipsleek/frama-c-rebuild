/* run.config*
   EXIT: 1
   STDOPT:
*/

void main(void) {
    // This syntax error is reported on a long line range. Some lines will
    // be omitted.
    1 = 
    // 1
    // 2
    // 3
    // 4
    // 5
    // 6
    // 7
    // 8
    // 9
    // 10
    2;
}
