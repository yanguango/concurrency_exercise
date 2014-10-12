#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "monitor.h"
#include "cart.h"

void monitor_init() {
}

void monitor_arrive(struct cart_t *cart) {
    printf("Cart %d in direction %c arrive at intersection\n", cart->num, cart->dir);
}

void monitor_cross(struct cart_t *cart) {
    printf("Cart %d in direction %c enter intersection\n", cart->num, cart->dir);
    sleep(1);
    printf("Cart %d in direction %c cross intersection\n", cart->num, cart->dir);
}

void monitor_leave(struct cart_t *cart) {
    printf("Cart %d in direction %c leave intersection\n", cart->num, cart->dir);
}

void monitor_shutdown() {
}
