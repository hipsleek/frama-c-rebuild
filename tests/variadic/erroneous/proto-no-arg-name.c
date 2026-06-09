/* run.config*
   STDOPT: #"-check" +"-print"
*/
/* Test that the AST check is true after translation, even if no argument name
   is in the declared function. */

// Declare prototypes without argument names
extern int __va_fcntl_void(int, int);
extern int __va_fcntl_int(int, int, int);
struct flock;
extern int __va_fcntl_flock(int, int, struct flock *);
extern int fcntl(int, int, ...);
extern int execv(const char *, char *const *);
extern int execl(const char *, const char *, ...);
extern int printf(const char*, ...);

int main() {
  // Overload translation
  fcntl(0, 1);

  // Aggregator translation
  execl("ls", "ls", "--reverse", 0);

  // Format fun translation
  printf("%d", 42);

  // Wrong format to trigger fallback translation
  printf("%-- le", 2);

  return 0;
}
