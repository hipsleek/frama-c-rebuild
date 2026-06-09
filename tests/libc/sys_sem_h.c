#include <stdio.h>
#include <sys/sem.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdlib.h>

// Based on SEMOP(3P) manpage

int main() {
  key_t semkey;
  int semid;
  struct sembuf sbuf;
  union semun {
    int val;
    struct semid_ds *buf;
    unsigned short *array;
  } arg;
  struct semid_ds ds;
  /* Get unique key for semaphore. */
  if ((semkey = ftok("/tmp", 'a')) == (key_t) -1) {
    perror("IPC error: ftok"); exit(1);
  }

  /* Get semaphore ID associated with this key. */
  if ((semid = semget(semkey, 0, 0)) == -1) {

    /* Semaphore does not exist - Create. */
    if ((semid = semget(semkey, 1, IPC_CREAT | IPC_EXCL | S_IRUSR |
                        S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH)) != -1)
      {
        /* Initialize the semaphore. */
        arg.val = 0;
        sbuf.sem_num = 0;
        sbuf.sem_op = 2;  /* This is the number of runs without queuing. */
        sbuf.sem_flg = 0;
        if (semctl(semid, 0, SETVAL, arg) == -1
            || semop(semid, &sbuf, 1) == -1) {
          perror("IPC error: semop"); exit(1);
        }
      }
    else if (errno == EEXIST) {
      if ((semid = semget(semkey, 0, 0)) == -1) {
        perror("IPC error 1: semget"); exit(1);
      }
      goto check_init;
    }
    else {
      perror("IPC error 2: semget"); exit(1);
    }
  }
  else
    {
      /* Check that semid has completed initialization. */
      /* An application can use a retry loop at this point rather than
         exiting. */
    check_init:
      arg.buf = &ds;
      if (semctl(semid, 0, IPC_STAT, arg) < 0) {
        perror("IPC error 3: semctl"); exit(1);
      }
      if (ds.sem_otime == 0) {
        perror("IPC error 4: semctl"); exit(1);
      }
    }
  sbuf.sem_num = 0;
  sbuf.sem_op = -1;
  sbuf.sem_flg = SEM_UNDO;
  if (semop(semid, &sbuf, 1) == -1) {
    perror("IPC Error: semop"); exit(1);
  }
  return 0;
}
