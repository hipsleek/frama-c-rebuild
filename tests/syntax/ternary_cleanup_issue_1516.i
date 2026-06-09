/* run.config
   STDOPT:
*/

/* In ternary conditions "e1 ? e2 : e3", cabs2cil first evaluate the condition
   expression "ce1" from e1, and then decide what to do with the ternary
   depending on ce1 value and e2/e3 statuses (are their chunks empty ? are they
   dangerous ? Is e2 omitted ? etc.). In some cases, we do not use ce1 in the
   final result, so we need to make sure that locals created when computing ce1
   are removed from function locals.
*/

int x = 0;

int f() { x += 1; return x; }

void main(){

  // Dropping ce1, e2 and e3, and retyping e1 only without the need to store
  // its result in a tmp variable.
  f() ? 1: 2;

  // Same as above, in case where e2 is omitted
  f() ? : 42;

  // Same again without dropping the result of the ternary
  int y = f() ? : f();

  // Keep ce1 and e3
  f() ? : f();

  return;
}

