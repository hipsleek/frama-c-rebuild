/*  run.config*
    COMMENT: Test tricky case in dataflow analysis.
*/

int a;

int f(void) {
    return a;
}

int main(void) {
    int c;
    // In the two calls below, the dataflow analysis first visits the one
    // without a use of the return value and then the one with the use of
    // the return value. In the latter case, we must reanalyze the function
    // body in the light of the new information that the return value is
    // monitored, even if the dataflow information at the exit of both calls
    // is equal, containing just [c]. The reanalysis is needed to
    // propagate the monitoring status from [c] to [a].
    c = f();
    f();
    /*@ assert security_status(c) == public; */
    return 0;
}
