CC = gcc
CFLAGS = -g2 -Wall -lm
# CFLAGS = -O2 -DNDEBUG -lm

all: uts

uts: uts.c instruc-cases.c
	$(CC) $(CFLAGS) uts.c gc/gc.a -o uts

instruc-cases.c: instrucs.c make-instrucs.awk
	awk -f make-instrucs.awk instrucs.c >instruc-cases.c
