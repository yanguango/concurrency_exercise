#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

int* SieveOfEratosthenes(int n) {
    int top_value = n;
    int count = top_value - 1;
    int* array = calloc(top_value + 1, sizeof(int));
    int i, prime, multiple;
    /* mark each int as potentially prime */
    for (i = 2; i <= top_value; ++i)
        array[i] = 1;
    /* for each starting prime, mark its every multiple as non-prime */
    for (prime = 2; prime <= top_value; ++prime) {
        if (array[prime])
            for (multiple = 2 * prime; multiple <= top_value; multiple += prime)
                if (array[multiple]) {
                    array[multiple] = 0;
                    --count;
                }
    }
    return array;
}


int main(int argc, char **argv) {
    int i;
    int fields[2];
    int* primeArray = NULL;
    int exitCode = 3;
    int status;
    
    pipe(fields); 
        
    if (argc < 2) {
        printf("usage: ./primes <increasing positive integers>\n");
    } else {
        primeArray = SieveOfEratosthenes(strtol(argv[argc - 1], NULL, 10));
        
        for (i = 1; argv[i] != NULL; ++i) {
            int bottom = ((i == 1) ? 2 : strtol(argv[i - 1], NULL, 10) + 1);
            int top = strtol(argv[i], NULL, 10);
            
            pid_t child = fork();
            if ((int)child < 0) {
                fprintf(stderr, "fork error!\n");
                exit(0);
            } else if ((int)child == 0) {
                int j;
                int flag = -1; /* write -1 to pipe to indicate child process exit */
                int childPid = (int)getpid();

                close(fields[0]);
                for (j = bottom; j <= top; ++j) {
                    if (primeArray[j]) {
                        write(fields[1], &j, sizeof(int));
                    }
                }
                write(fields[1], &flag, sizeof(int));
                write(fields[1], &childPid, sizeof(int));

                exit(exitCode);
            } else {
              printf("child %d: bottom=%d, top=%d\n", (int)child, bottom, top);
             }
        }
    }

    close(fields[1]);
    while(1) {
        int readInt;
        if (read(fields[0], &readInt, sizeof(int)) > 0) {
            if (readInt == -1) {
                read(fields[0], &readInt, sizeof(int));
                (void)waitpid(readInt, &status, 0);
                if (WEXITSTATUS(status) == exitCode) {
                    printf("child %d exited cleanly\n", readInt);
                }
            } else {
                printf("%d is prime\n", readInt);
            }
        } else {
            break;
        }
    }

  return 0;
}

