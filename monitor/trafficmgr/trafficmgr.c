#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include "q.h"
#include "cart.h"
#include "monitor.h"

pthread_t n_thread;
pthread_t w_thread;
pthread_t s_thread;
pthread_t e_thread;
pthread_mutex_t monitor;
pthread_cond_t activeN;
pthread_cond_t activeW;
pthread_cond_t activeS;
pthread_cond_t activeE;

char activeDir;
char *dirs;

char next_dir(char dir) {
    int pos = (int)(strchr(dirs, dir) - dirs);
    return dirs[(pos + 1) % strlen(dirs)];
}

void del_dir(char dir) {
    int i;
    for(i = 0; i < strlen(dirs); i++ )
        if( dirs[i] == dir )
            strcpy( dirs + i, dirs + i + 1 );
}

pthread_cond_t *dir_cond(char dir) {
    pthread_cond_t *cond;
    switch(dir) {
        case Q_NORTH:
            cond = &activeN;
            break;
        case Q_WEST:
            cond = &activeW;
            break;
        case Q_SOUTH:
            cond = &activeS;
            break;
        case Q_EAST:
            cond = &activeE;
            break;
    }
    return cond;
}

void *traffic(void *params) {
    char dir = *((char *)params);
    char nextDir;
    pthread_cond_t *cond;
    pthread_cond_t *nextDirCond;
    struct cart_t *cart;
    cond = dir_cond(dir);
        
    fprintf(stderr, "Thread for direction %c starts\n", dir);
    cart = q_getCart(dir);
    while (cart != NULL) {
        fprintf(stderr, "Thread for direction %c gets cart %i\n", dir, cart->num);

        pthread_mutex_lock(&monitor); 
        while(activeDir != dir){
            pthread_cond_wait(cond, &monitor);
            printf("Thread for direction %c must wait for entering\n", dir);
        }
        printf("Thread for direction %c is allowed to enter\n", dir);
        
        monitor_arrive(cart);
        monitor_cross(cart);
        monitor_leave(cart);

        nextDir = next_dir(dir);
        nextDirCond = dir_cond(nextDir);
        activeDir = nextDir; 
        
        pthread_cond_signal(nextDirCond);
        printf("Thread for direction %c signal thread for direction %c\n", dir, nextDir);

        pthread_mutex_unlock(&monitor);
        
        cart = q_getCart(dir);
    }
    del_dir(dir);
    fprintf(stderr, "Thread for direction %c exits\n", dir);
    return (void *)NULL;
}

int main(int argc, char **argv) {
    int i;
    char *cartstring;
    char dirN = Q_NORTH;
    char dirS = Q_SOUTH;
    char dirW = Q_WEST;
    char dirE = Q_EAST;
    dirs = calloc(5, sizeof(char));
    strcpy(dirs, "nwse");
    
    if (argc < 2) {
        printf("usage: ./trafficmgr <cart string>\n");
    } else {
        cartstring = argv[1];
        for (i = 0; i < strlen(cartstring); i++){
            q_putCart(cartstring[i]);
        }
                
    }

    activeDir = Q_EAST;
        
    if (pthread_create(&n_thread, NULL, traffic, (void *)&dirN) != 0)
        perror("pthread_create"), exit(1);
    if (pthread_create(&w_thread, NULL, traffic, (void *)&dirW) != 0)
        perror("pthread_create"), exit(1);
    if (pthread_create(&s_thread, NULL, traffic, (void *)&dirS) != 0)
        perror("pthread_create"), exit(1);
    if (pthread_create(&e_thread, NULL, traffic, (void *)&dirE) != 0)
        perror("pthread_create"), exit(1);

    if (pthread_join(n_thread, NULL) != 0) {
        perror("pthread_join"), exit(1);
    }
    if (pthread_join(w_thread, NULL) != 0) {
        perror("pthread_join"), exit(1);
    }
    if (pthread_join(s_thread, NULL) != 0) {
        perror("pthread_join"), exit(1);
    }
    if (pthread_join(e_thread, NULL) != 0) {
        perror("pthread_join"), exit(1);
    }
     return 0;
}
