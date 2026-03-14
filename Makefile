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

utsvm: utsvm.c config.h opcodes.h prims.h byteops.c
	$(CC) $(CFLAGS) $(GC_CFLAGS) utsvm.c $(GC_LIBS) -lm -o utsvm

opcodes.h opcodes.scm: build-instrucs-tables.awk instrucs
	awk -f build-instrucs-tables.awk instrucs

prims.h primcodes.scm: build-prim-codes.awk prims
	awk -f build-prim-codes.awk prims

clean:
	rm -f utsvm
	rm -f opcodes.h opcodes.scm primcodes.h primcodes.scm
	rm -f test/tmp[123]

install: utsvm uts.fasl
	mkdir -p $(PREFIX)/lib/uts $(PREFIX)/bin
	cp utsvm uts.fasl $(PREFIX)/lib/uts/
	printf '#!/bin/sh\nexec $(PREFIX)/lib/uts/utsvm $(PREFIX)/lib/uts/uts.fasl "$$@"\n' > $(PREFIX)/bin/uts
	chmod +x $(PREFIX)/bin/uts

uninstall:
	rm -rf $(PREFIX)/lib/utsvm $(PREFIX)/bin/uts

.PHONY: all clean install uninstall
