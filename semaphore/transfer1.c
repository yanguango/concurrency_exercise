#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <semaphore.h>
#include <pthread.h>
#include <string.h>

#define MAXLINES 1024
#define MAXLINESIZE 128

sem_t mutex;
char *buffer[MAXLINES];
FILE *in;
FILE *out;
int usec;
pthread_t consumer_thread;

/* read text line from input file and write to buffer */
void produce() {
    size_t len = 0;
    ssize_t curr_line_len;
    char *line = NULL;
    int lineIndex = 0;
    int readToEnd = 0;
    
    while (readToEnd == 0) {
        curr_line_len = getline(&line, &len, in);

        if (sem_wait(&mutex) < 0)
            perror("producer sem_wait"), exit(1);
        
        if (curr_line_len == -1) {
            strcpy(buffer[lineIndex], "QUIT");
            printf("fill thread: wrote QUIT into buffer\n");
            readToEnd = 1;
        } else {
            strcpy(buffer[lineIndex], line);
            lineIndex++;
            printf("fill thread: wrote [%s] into buffer\n", line);
        }
        
        if (sem_post(&mutex) < 0)
            perror("producer sem_post"), exit(1);
        usleep(usec);
    }
    
}

/* consumer thread */
void *consumer(void *params) {
    int readIndex = 0;
    
    while(1){
        if (sem_wait(&mutex) < 0)
            perror("consumer sem_wait"), exit(1);
        
        if (strlen(buffer[readIndex]) == 0) {
            printf("drain thread: no new string in buffer\n");
        } else {
            printf("drain thread: read [%s] from buffer\n", buffer[readIndex]);
        
            if (strcmp(buffer[readIndex], "QUIT") == 0){
                pthread_exit(0);
            } else {
                fwrite(buffer[readIndex], 1, strlen(buffer[readIndex]), out);
            }

            readIndex++;
        }
        
        if (sem_post(&mutex) < 0)
            perror("consumer sem_post"), exit(1);
    }
    return (void *)NULL;
}
    
int main(int argc, char* argv[]) {
    int i;
        
    if (argc != 4) {
        fprintf(stderr, "USAGE:./transfer1 <INPUT> <OUTPUT> <USEC>\n");
        exit(0);
    }
    
    usec = strtol(argv[3], NULL, 10);
    
    for (i = 0; i < MAXLINES; i++)
        buffer[i] = calloc(MAXLINESIZE, sizeof(char));

    in = fopen(argv[1], "r");
    out = fopen(argv[2], "w");
    if (in == NULL || out == NULL) {
        exit(EXIT_FAILURE);
    }

    if (sem_init(&mutex, 0, 1) < 0)
        perror("mutex semaphore init"), exit(1);
   
    /* consumer thread */
    if (pthread_create(&consumer_thread, NULL, consumer, NULL) != 0)
        perror("pthread_create"), exit(1);
    
    /* producer thread */ 
    produce();

    if (pthread_join(consumer_thread, NULL) != 0) {
        /* consumer thread terminate fail */
        perror("pthread_join"), exit(1);
    } else {
        /* consumer thread terminate success */
        for (i = 0; i < MAXLINES; i++) {
            free(buffer[i]);
        }
        if (sem_destroy(&mutex) < 0)
            perror("sem_destroy"), exit(1);
    }

    fclose(in);
    fclose(out);

    return 0;
}


