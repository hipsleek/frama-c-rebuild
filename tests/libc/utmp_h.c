#include <pwd.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <utmp.h>

// extract based on the getutent(3) man page, 2023-10-31

int main(void) {
  struct utmp entry;

  system("echo before adding entry:;who");

  entry.ut_type = USER_PROCESS;
  entry.ut_pid = getpid();
  strcpy(entry.ut_line, ttyname(STDIN_FILENO) + strlen("/dev/"));
  /* only correct for ptys named /dev/tty[pqr][0-9a-z] */
  strcpy(entry.ut_id, ttyname(STDIN_FILENO) + strlen("/dev/tty"));
  entry.ut_time = time(NULL);
  strcpy(entry.ut_user, getpwuid(getuid())->pw_name);
  memset(entry.ut_host, 0, UT_HOSTSIZE);
  entry.ut_addr = 0;
  setutent();
  pututline(&entry);

  system("echo after adding entry:;who");

  entry.ut_type = DEAD_PROCESS;
  memset(entry.ut_line, 0, UT_LINESIZE);
  entry.ut_time = 0;
  memset(entry.ut_user, 0, UT_NAMESIZE);
  setutent();
  pututline(&entry);

  system("echo after removing entry:;who");

  endutent();
  return EXIT_SUCCESS;
}
