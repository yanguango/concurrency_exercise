CC = gcc
CFLAGS = -Wall -pedantic-errors
LIBS = -lpthread
SRCS = trafficmgr.c monitor.c q.c
OBJS = $(SRCS:.c=.o)
MAIN = trafficmgr

$(MAIN): $(OBJS) 
	$(CC) $(CFLAGS) -o $(MAIN) $(OBJS)  $(LIBS)
clean:
	rm -rf *.o trafficmgr
