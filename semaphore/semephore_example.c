#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <pthread.h>
#include <semaphore.h>


sem_t lock;
#define BUFFER_SIZE 1024
char *buffer;


void *start(void *args) {
    int numStrings, bufptr, i, numBytes;
    int *retval;

    if (sem_wait(&lock) < 0)
        perror("2nd thread sem_wait"), exit(1);
    bufptr = 0;
    bcopy(buffer, (void *)&numStrings, sizeof(int));
    bufptr += sizeof(int);
    numBytes = 0;
    for (i=0; i<numStrings; i++) {
        char nextString[BUFFER_SIZE];

        strcpy(nextString, &buffer[bufptr]);
	bufptr += strlen(nextString) + 1;
	numBytes += printf("%s\n", nextString) - 1;
    }
    if (sem_post(&lock) < 0)
        perror("2nd thread sem_post"), exit(1);

/* return value must NOT be allocated on the stack */
    retval = (int *) malloc(sizeof(int));
    *retval = numBytes;
    return (void *)retval;
}


int main(int argc, char* argv[]) {
    int bufptr, i;
    pthread_t tid;
    void *retvalLocation;

    if (argc < 2)
        fprintf(stderr, "usage: %s strings\n", argv[0]), exit(0);

    if (sem_init(&lock, 0, 1) < 0)
        perror("semaphore initialization"), exit(1);
/* assume BUFFER_SIZE is big enough for all the strings */
    if ((buffer = calloc(BUFFER_SIZE, 1)) == NULL)
        perror("buffer initialization"), exit(1);

    if (sem_wait(&lock) < 0)
        perror("sem_wait"), exit(1);
    bufptr = 0;
    argc--;
    bcopy((void *)&argc, buffer, sizeof(int));
    bufptr += sizeof(int);
    for (i=1; i<=argc; i++) {
        strcpy(&buffer[bufptr], argv[i]);
	bufptr += strlen(argv[i]) + 1;
    }
    if (sem_post(&lock) < 0)
        perror("sem_post"), exit(1);

    if (pthread_create(&tid, NULL, start, NULL) != 0)
        perror("pthread_create"), exit(1);
    if (pthread_join(tid, &retvalLocation) != 0)
        perror("pthread_join"), exit(1);
    printf("second thread returned %i\n", *(int *)retvalLocation);
    if (sem_destroy(&lock) < 0)
        perror("sem_destroy"), exit(1);

    return 0;
}
