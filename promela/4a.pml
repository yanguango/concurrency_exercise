#define FREE 0
#define NO_CART 0
#define MAX_CARTS 16
#define MAX_DIR_CARTS 3

byte carts[MAX_CARTS];

byte launchPad = FREE;
byte intersection = FREE;

proctype moveCarts(byte start) {    
    byte i = start;
    byte finished[MAX_DIR_CARTS];

    do
    :: carts[i] == NO_CART -> break;
    :: else ->
        printf("thread %d move cart %d\n", _pid, carts[i]);
		
	/* arrive */
        atomic { 
            launchPad == FREE;
            launchPad = carts[i];
        }
        
	/* cross */
        atomic {
            intersection == FREE;
            intersection = carts[i];
            launchPad = FREE;
        }
        
	/* leave */
        atomic {
            intersection = FREE;
        }
		
	finished[i - start] = carts[i];
        i++
    od;
    
    byte j = 0;
    do
    :: carts[j + start] == NO_CART -> break
    :: else ->
        assert(carts[j + start] == finished[j]);
	j++
    od
}


init {
    /*
     *  carts: ewesnewnsnws
     *  carts array: [5, 8, 10, 2, 7, 11, 4, 9, 12, 1, 3, 6]
     */

    /* carts in north */
    carts[0] = 5;
    carts[1] = 8;
    carts[2] = 10;
    carts[3] = NO_CART;
    
    /* carts in west */
    carts[4] = 2;
    carts[5] = 7;
    carts[6] = 11;
    carts[7] = NO_CART;
  
    /* carts in south */
    carts[8] = 4;
    carts[9] = 9;
    carts[10] = 12;
    carts[11] = NO_CART;

    /* carts in east */
    carts[12] = 1;
    carts[13] = 3;
    carts[14] = 6;
    carts[15] = NO_CART;
	
    atomic {
        run moveCarts(0);
        run moveCarts(4);
        run moveCarts(8);
        run moveCarts(12);
    }  
}

 

