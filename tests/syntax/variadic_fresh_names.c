/* run.config
STDOPT:
*/
#include "stdio.h"

/*@ requires valid_read_string(format);
    assigns \result, stream->__fc_FILE_data;
    assigns \result
      \from (indirect: stream->__fc_FILE_id),
            (indirect: stream->__fc_FILE_data),
            (indirect: *(format + (0 ..))), (indirect: param0);
    assigns stream->__fc_FILE_data
      \from (indirect: stream->__fc_FILE_id), stream->__fc_FILE_data,
            (indirect: *(format + (0 ..))), param0;
 */
int fprintf_va_1(FILE * restrict stream, char const * restrict format,
                 int param0);

void main(void)
{
  fprintf(__fc_stderr,"%d ",42); /* fprintf_va_1 */
  return;
}
