#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include "q.h"
#include "cart.h"
#include "monitor.h"

static pthread_t n_thread;
static pthread_t w_thread;
static pthread_t s_thread;
static pthread_t e_thread;

static pthread_mutex_t monitor;

/* four condition variable for each direction */
static pthread_cond_t activeN;
static pthread_cond_t activeW;
static pthread_cond_t activeS;
static pthread_cond_t activeE;

static pthread_barrier_t barrier;

/* indicate next active direction */
char activeDir; 

/* a directions string for obtaining next direction
 * follow the counterclock order */
char *dirs; 

/* get next direction */
char next_dir(char dir) {
    int pos = (int)(strchr(dirs, dir) - dirs);
    return dirs[(pos + 1) % strlen(dirs)];
}

/* delete one direction when carts in the direction all entered */
void del_dir(char dir) {
    int i;
    for(i = 0; i < strlen(dirs); i++ )
        if( dirs[i] == dir )
            strcpy( dirs + i, dirs + i + 1 );
}

/* get the condition variable accoring to the direction */
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

/* main procedure of traffic thread in four directions */
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
    
    /* flag all carts in the queue have entered */
    q_cartHasEntered(dir);
    /* delete current direction in dirs */
    del_dir(dir);
    
    pthread_barrier_wait(&barrier);
    
    fprintf(stderr, "Thread for direction %c exits\n", dir);
    return (void *)NULL;
}

int main(int argc, char **argv) {
    int i;
    char dirN = Q_NORTH;
    char dirS = Q_SOUTH;
    char dirW = Q_WEST;
    char dirE = Q_EAST;
    
    /* initialize dirs with nwse, counterclock order */    
    dirs = calloc(5, sizeof(char));
    strcpy(dirs, "nwse");
    

    if (argc < 2) {
        printf("usage: ./trafficmgr <cart string>\n");
    } else {
        /* read carts into queues of four directions */
        for (i = 0; i < strlen(argv[1]); i++){
            q_putCart(argv[1][i]);
        } 
    }
    
    pthread_barrier_init(&barrier, NULL, 4);

    /* print queues in each direction before traffic */
    q_print(dirN);
    q_print(dirS);
    q_print(dirW);
    q_print(dirE);
    
    /* set the first active direction, it can be set to anyone direction */
    activeDir = Q_EAST;
        
    /* create threads for every direction */
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

    /* free used resource */
    q_shutdown();
    monitor_shutdown();
    if(pthread_barrier_destroy(&barrier))
        perror("pthread_barrier_destroy"), exit(1);

    return 0;
}
