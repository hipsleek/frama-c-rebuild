/* run.config
PLUGIN: @PTEST_PLUGIN@ eva,scope
STDOPT: -acsl-import %{dep:./@PTEST_NAME@.acsl} -then -eva
 */


typedef enum { STATUS_KO=0, STATUS_OK=1 } STATUS ;

STATUS status[2] = { STATUS_KO, STATUS_OK} ;
STATUS * p_status = & status ;

int x;

void nop() {
  return;
}

void main () {
  nop();
}
