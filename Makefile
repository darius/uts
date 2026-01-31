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

all: utsvm

utsvm: utsvm.c instruc-cases.c config.h
	$(CC) $(CFLAGS) $(GC_CFLAGS) utsvm.c $(GC_LIBS) -lm -o utsvm

instruc-cases.c: instrucs.c make-instrucs.awk
	awk -f make-instrucs.awk instrucs.c >instruc-cases.c

clean:
	rm -f utsvm instruc-cases.c

install: utsvm uts.fasl
	mkdir -p $(PREFIX)/lib/uts $(PREFIX)/bin
	cp utsvm uts.fasl $(PREFIX)/lib/uts/
	printf '#!/bin/sh\nexec $(PREFIX)/lib/uts/utsvm $(PREFIX)/lib/uts/uts.fasl "$$@"\n' > $(PREFIX)/bin/uts
	chmod +x $(PREFIX)/bin/uts

uninstall:
	rm -rf $(PREFIX)/lib/utsvm $(PREFIX)/bin/uts

.PHONY: all clean install uninstall
