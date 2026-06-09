/* run.config*
   STDOPT: +"-eva-secure-flow -eva-msg-key=d-taint"
 */

#define __fc_private __attribute__((private))
#define __fc_public __attribute__((public))

extern int __fc_private secret;

void annot(void) {
  // Non-private variables are considered public by security_status
  unsigned int __fc_public non_secret;
  unsigned int non_classified;

  //@ check security_status(secret) == private;
  //@ check security_status(non_secret) == public;
  //@ check security_status(non_classified) == public;
}

void direct_interference(void) {
  // Do not warn about direct non-interference violations on variables without a
  // specified security level
  unsigned int __fc_public non_secret;
  unsigned int non_classified;

  non_secret = secret;     // Should warn
  non_classified = secret; // Should not warn
}

void indirect_interference(void) {
  // Do not warn about indirect non-interference violations on variables without
  // a specified security level. Also warn on conditions depending on private
  // data
  unsigned int __fc_public non_secret;
  unsigned int non_classified;

  // Should warn about condition
  if (secret)
    non_secret = 1; // Should warn
  else
    non_classified = 1; // Should not warn
}

/*@ assigns *x \from *x;
  @ admit ensures security_status(*x) == public; */
void sanitize(int *x);

void public_after_sanitize(void) {
  int non_classified = secret;

  //@ check security_status(non_classified) == public;
  sanitize(&non_classified);
  //@ check security_status(non_classified) == public;
}

/*@ requires security_status(x) == private && security_status(y) == public;
  @ admit ensures security_status(\result) == private;
  @ assigns \result \from x, y;
 */
int add(unsigned int x, unsigned int y) {
  //@ check security_status(x) == private;
  //@ check security_status(y) == public;
  int result = x + y;
  /*@ check security_status(result) == private; */
  return result;
}

void annot_contract(void) {
  unsigned int non_classified = 1;
  unsigned int r = add(secret, non_classified);
  /*@ check security_status(r) == private; */
}

/*@ assigns \result \from *x, x; */
extern int assigns_from(int *x);

void direct_interference_from_assigns(void) {
  int zero = 0, one = 1;

  // Should warn on the condition
  int *ptr = (secret ? &zero : &one);
  /*@ check security_status(zero) == public; */
  /*@ check security_status(one) == public; */
  /*@ check security_status(ptr) == private; */

  int __fc_public v = assigns_from(ptr); // Should warn
  /*@ check security_status(v) == private; */
}

void indirect_interference_goto() {
  int __fc_public x, y, z;

  // Should warn on condition
  if (secret > 0) {
    x = 1; // Should warn
    goto L;
  } else {
  // This is always executed, 'y' must remain public
  L:
    y = 0;
  }
  z = 1;
  /*@ check security_status(y) == public; */
  /*@ check security_status(z) == public; */
  /*@ check security_status(x) == private; */
}

void indirect_interference_ptr_array(void) {
  int __fc_public x, y, z;
  int *ptr_array[2] = {&x, &y};
  int *q;

  /*@ check security_status(x) == public; */
  /*@ check security_status(y) == public; */
  /*@ check security_status(ptr_array) == public; */

  int secret_idx = !!secret;
  /*@ check security_status(secret_idx) == private; */
  *ptr_array[secret_idx] = 2;
  /*@ check security_status(*ptr_array[secret_idx]) == private; */
  /*@ check security_status(x) == private; */
  /*@ check security_status(y) == private; */

  /* We can make both x and y public again. */
  x = 0;
  y = 0;
  /*@ check security_status(x) == public; */
  /*@ check security_status(y) == public; */
  /*@ check security_status(ptr_array) == public; */
  z = *ptr_array[secret_idx];
  /*@ check security_status(z) == private; */
  /*@ check security_status(x) == public; */
  /*@ check security_status(y) == public; */
}

void set_y(int *y) { *y = 1; }

void set_xy(int *x, int *y) {
  *x = 1;
  if (secret) {
    set_y(y);
  }
}

void indirect_interference_fun_call(void) {
  int __fc_public x;
  int y;

  /*@ check security_status(x) == public; */
  /*@ check security_status(y) == public; */
  set_xy(&x, &y);
  /*@ check security_status(x) == public; */
  /*@ check security_status(y) == private; */

  y = 0;
  /*@ check security_status(y) == public; */
  if (secret) {     // Should warn on condition
    set_xy(&x, &y); // Should warn on x
    /*@ check security_status(x) == private; */
  }
  /*@ check security_status(x) == private; */
  /*@ check security_status(y) == private; */

  set_xy(&x, &y);
  /*@ check security_status(x) == public; */
}

void direct_interference_array(void) {
  int __fc_public array[5];

  //@ loop unroll 5;
  for (int i = 0; i < 5; i++)
    if (i % 2 == 0) {
      array[i] = secret;
    }

  //@ check security_status(array) == private;
  //@ check security_status(array[0]) == private;
  //@ check security_status(array[1]) == public;
  //@ check security_status(array[2]) == private;
  //@ check security_status(array[3]) == public;
  //@ check security_status(array[4]) == private;
}

struct pair {
  int a[5];
  double b;
};

struct nested {
  int n;
  struct pair p;
};

void indirect_interference_struct(void) {
  struct nested s = {0, {{0, 1, 2}, 2.0}};
  struct nested t = {3, {{4}, 5.0}};
  struct nested u;
  struct nested __fc_public v;

  //@ check security_status(s.n) == public;
  //@ check security_status(s.p.a) == public;
  //@ check security_status(s.p.b) == public;
  //@ check security_status(t.n) == public;
  //@ check security_status(t.p.a) == public;
  //@ check security_status(t.p.b) == public;
  if (secret) { // Should warn on condition
    u = s;
  } else {
    u = t;
  }
  //@ check security_status(u.n) == private;
  //@ check security_status(u.p.a) == private;
  //@ check security_status(u.p.b) == private;

  v = u; // Should warn
  //@ check security_status(v) == private;
  v.n = 0;
  v.p.a[2] = v.p.a[3] = 0;
  //@ check security_status(v.n) == public;
  //@ check security_status(v.p.a) == private;
  //@ check security_status(v.p.a[2]) == public;
  //@ check security_status(v.p.a[3]) == public;
}

void interference_on_private_only(void) {
  int x = 0, y = 0;
  //@ taint toto:x;
  int __fc_public result1, result2;

  result1 = x;
  //@ check security_status(result1) == public;
  //@ assert security_status(result1) == public;

  if (result1)
    result2 = x;
  else
    result2 = y;
  //@ check security_status(result2) == public;
  //@ assert security_status(result2) == public;
}

int main(void) {
  annot();
  direct_interference();
  indirect_interference();
  public_after_sanitize();
  annot_contract();
  direct_interference_from_assigns();
  indirect_interference_goto();
  indirect_interference_ptr_array();
  indirect_interference_fun_call();
  direct_interference_array();
  indirect_interference_struct();
  interference_on_private_only();
  return 0;
}
