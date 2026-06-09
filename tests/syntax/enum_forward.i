/* run.config
   OPT: -machdep gcc_x86_64 -print
   EXIT: 1
   STDOPT:
*/
/* forward declaration of enum is supported by GCC but nonstandard */

enum e X;
enum e { V };

enum g Y;
typedef enum g { W } g;

enum h;
typedef enum h h;
typedef enum h { Z } h;
h hh = Z;

typedef enum i { A } i;
typedef enum i i;
i ii = A;
