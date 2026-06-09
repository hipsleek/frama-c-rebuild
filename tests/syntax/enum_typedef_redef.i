/* run.config
   EXIT: 1
   STDOPT:
*/

/* this is the counterpart of enum_forward.i: we can accept a double typedef
   with of an enum only if the first type is a forward declaration. */

typedef enum h { Z } h;
typedef enum h { Z } h;
h hh = Z;

// other problematic case, even though the enum is compatible with int

typedef enum { E } t;
typedef int t;

typedef int q;
typedef enum { F } q;
