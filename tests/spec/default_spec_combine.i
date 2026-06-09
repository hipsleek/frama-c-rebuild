/* run.config

   MODULE: @PTEST_NAME@
   STDOPT: +"-generated-spec-mode donothing"
   STDOPT: +"-generated-spec-mode safe"
   STDOPT: +"-generated-spec-mode frama-c"
*/

/*@ axiomatic a {
  @ predicate P reads \nothing;
  @ predicate Q reads \nothing;
  @ predicate R reads \nothing;
  @ predicate S reads \nothing;
  }*/


// Test combine exits with unguarded behaviors and no complete
/*@ behavior A:
  @   exits P;
  @ behavior B:
  @   exits Q;
  @   exits R;
  @ behavior C:
  @   assumes R;
  @   exits S;
  @*/
void f1_unguarded(void);

// Test combine exits with only guarded behaviors and complete
// Also test when complete behaviors have no clauses exits
/*@ behavior A:
  @   assumes P;
  @ behavior B:
  @   assumes Q;
  @   exits Q;
  @   exits R;
  @ behavior C:
  @   assumes R;
  @   exits S;
  @ behavior D:
  @   assumes S;
  @ complete behaviors A, D;
  @ complete behaviors B, C;
  @*/
void f1_complete(void);

// Test combine exits with only guarded behaviors and no complete
/*@ behavior A:
  @   assumes P;
  @   exits P;
  @ behavior B:
  @   assumes Q;
  @   exits Q;
  @   exits R;
  @ behavior C:
  @   assumes R;
  @   exits S;
  @*/
void f1_guarded(void);

// Test combine assigns with unguarded behaviors and no complete
/*@ behavior A:
  @   assigns *a;
  @ behavior B:
  @   assigns *b;
  @   assigns *a \from c;
  @   assigns *a \from d;
  @   assigns *b \from c;
  @ behavior C:
  @   assumes R;
  @   assigns *a, *e;
  @*/
void f2_unguarded(int* a, int *b, int c, int d, int *e);

// Test combine assigns with only guarded behaviors and complete
/*@ behavior A:
  @   assumes P;
  @   assigns *a;
  @ behavior B:
  @   assumes Q;
  @   assigns *b;
  @   assigns *a \from c;
  @   assigns *a \from d;
  @   assigns *b \from c;
  @ behavior C:
  @   assumes R;
  @   assigns *a, *e;
  @ complete behaviors;
  @*/
void f2_complete(int* a, int *b, int c, int d, int *e);

// Test combine assigns with only guarded behaviors and no complete
/*@ behavior A:
  @   assumes P;
  @   assigns *a;
  @ behavior B:
  @   assumes Q;
  @   assigns *b;
  @   assigns *a \from c;
  @   assigns *a \from d;
  @   assigns *b \from c;
  @ behavior C:
  @   assumes R;
  @   assigns *a, *e;
  @*/
void f2_guarded(int* a, int *b, int c, int d, int *e);


// Test combine requires with unguarded behaviors and no complete
/*@ behavior A:
  @   requires P;
  @   requires Q;
  @ behavior B:
  @   requires P;
  @   requires R;
  @   requires R;
  @ behavior C:
  @   assumes R;
  @   requires S;
  @*/
void f3_unguarded(void);

// Test combine requires with only guarded behaviors ans complete
/*@ behavior A:
  @   assumes P;
  @   requires P;
  @   requires Q;
  @ behavior B:
  @   assumes Q;
  @   requires P;
  @   requires R;
  @   requires R;
  @ behavior C:
  @   assumes R;
  @   requires S;
  @ complete behaviors;
  @*/
void f3_complete(void);

// Test combine requires with only guarded behaviors and no complete
/*@ behavior A:
  @   assumes P;
  @   requires P;
  @   requires Q;
  @ behavior B:
  @   assumes Q;
  @   requires P;
  @   requires R;
  @   requires R;
  @ behavior C:
  @   assumes R;
  @   requires S;
  @*/
void f3_guarded(void);

// Test combine unguarded frees/allocates and no complete
/*@ behavior A:
  @   allocates \result;
  @   allocates \old(a);
  @   frees b;
  @ behavior B:
  @   allocates \result;
  @   allocates \old(a);
  @   allocates \old(b);
  @   frees \nothing;
  @ behavior C:
  @   assumes R;
  @   allocates \nothing;
  @   frees a, b;
  @*/
int* f4_unguarded(int* a, int* b);

// Test combine only guarded frees/allocates and complete
/*@ behavior A:
  @   assumes P;
  @   allocates \result;
  @   allocates \old(a);
  @   frees b;
  @ behavior B:
  @   assumes Q;
  @   allocates \result;
  @   allocates \old(a);
  @   allocates \old(b);
  @   frees \nothing;
  @ behavior C:
  @   assumes R;
  @   allocates \nothing;
  @   frees a, b;
  @ complete behaviors;
  @*/
int* f4_complete(int* a, int* b);

// Test combine only guarded frees/allocates and no complete
/*@ behavior A:
  @   assumes P;
  @   allocates \result;
  @   allocates \old(a);
  @   frees b;
  @ behavior B:
  @   assumes Q;
  @   allocates \result;
  @   allocates \old(a);
  @   allocates \old(b);
  @   frees \nothing;
  @ behavior C:
  @   assumes R;
  @   allocates \nothing;
  @   frees a, b;
  @*/
int* f4_guarded(int* a, int* b);
