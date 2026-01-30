BASE_CFLAGS = -Wall

# Include generated config if it exists
-include config.mk

# Default to system paths if no config.mk
GC_CFLAGS ?=
GC_LIBS ?= -lgc
PREFIX ?= /usr/local

# Release by default, debug with: make DEBUG=1
ifdef DEBUG
  CFLAGS = $(BASE_CFLAGS) -g
else
  CFLAGS = $(BASE_CFLAGS) -g -O2 -DNDEBUG
endif

all: uts

uts: uts.c instruc-cases.c config.h
	$(CC) $(CFLAGS) $(GC_CFLAGS) uts.c $(GC_LIBS) -lm -o uts

instruc-cases.c: instrucs.c make-instrucs.awk
	awk -f make-instrucs.awk instrucs.c >instruc-cases.c

clean:
	rm -f uts instruc-cases.c

install: uts uts.fasl
	mkdir -p $(PREFIX)/lib/uts $(PREFIX)/bin
	cp uts uts.fasl $(PREFIX)/lib/uts/
	printf '#!/bin/sh\nexec $(PREFIX)/lib/uts/uts $(PREFIX)/lib/uts/uts.fasl "$$@"\n' > $(PREFIX)/bin/uts
	chmod +x $(PREFIX)/bin/uts

uninstall:
	rm -rf $(PREFIX)/lib/uts $(PREFIX)/bin/uts

.PHONY: all clean install uninstall
