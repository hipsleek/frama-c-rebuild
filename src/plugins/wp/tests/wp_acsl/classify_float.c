/* run.config_qualif
   OPT:
   OPT: -wp-model real
 */

/*@
  lemma NaN_not_finite: \forall double x; !( \is_NaN(x) && \is_finite(x) );
  lemma InfP_not_finite: \forall double x; !( \is_plus_infinity(x) && \is_finite(x) );
  lemma InfN_not_finite: \forall double x; !( \is_minus_infinity(x) && \is_finite(x) );
 */


#include <math.h>
