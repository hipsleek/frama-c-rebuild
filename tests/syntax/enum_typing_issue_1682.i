/* run.config
   STDOPT:
   STDOPT: +"-constfold"
*/

typedef enum {secfalse = 0x55aa55aa, sectrue = 0xaa55aa55} secbool;

secbool check_code_integrity(int x){
    if(x) return sectrue;
    return secfalse;
}

typedef enum { NONE = 0, FATAL = 1 } loglevel;

void log_printf(loglevel level);

void log_abort(void) {
	log_printf(FATAL);
}
