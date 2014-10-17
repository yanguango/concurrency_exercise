#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "monitor.h"
#include "cart.h"

void monitor_init() {
    /* All initialization are done in trafficmgr */
}

void monitor_arrive(struct cart_t *cart) {
    printf("Cart %d in direction %c arrive at intersection\n", cart->num, cart->dir);
}

void monitor_cross(struct cart_t *cart) {
    int i;
    printf("Cart %d in direction %c enter intersection\n", cart->num, cart->dir);
    printf("Cart %d in direction %c cross intersection ", cart->num, cart->dir);
    for(i = 0; i < 10; i++) {
        printf("%c%d", cart->dir, cart->num);
        fflush(stdout);
        sleep(1);
    }
    printf("\n");
}

void monitor_leave(struct cart_t *cart) {
    printf("Cart %d in direction %c leave intersection\n", cart->num, cart->dir);
}

void monitor_shutdown() {
}
