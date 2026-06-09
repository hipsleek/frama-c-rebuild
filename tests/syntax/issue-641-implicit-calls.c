/* run.config
    EXIT: 1
    STDOPT: #"-cpp-extra-args=-DINCOMP1"
    STDOPT: #"-cpp-extra-args=-DINCOMP2"
    STDOPT: #"-cpp-extra-args=-DINCOMP3"
    STDOPT: #"-cpp-extra-args=-DINCOMP4"
    STDOPT: #"-cpp-extra-args=-DINCOMP5"
    STDOPT: #"-cpp-extra-args=-DINCOMP6"
    STDOPT: #"-cpp-extra-args=-DINCOMP7"
*/

#ifdef INCOMP1
    void foo(unsigned x) { bar(bar(0, 12), x); }
#endif

#ifdef INCOMP2
    void foo(int x) { bar(bar(0), x); }
#endif

#ifdef INCOMP3
    void foo(int x) { bar(bar); }
#endif

#ifdef INCOMP4
    void foo(unsigned x) { bar((&bar)(0,12),x); }
#endif

#ifdef INCOMP5
    void foo(unsigned x) { bar((&bar)(0),x); }
#endif

#ifdef INCOMP6
    void foo(unsigned x) { bar(&bar); }
#endif

#ifdef INCOMP7
    void foo(unsigned x) { bar(x,bar(0, 12)); }
#endif
