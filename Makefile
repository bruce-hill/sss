CC=gcc
PREFIX=/usr/local
VERSION=0.12.1
CFLAGS=-std=c11 -Werror -D_XOPEN_SOURCE=700 -D_POSIX_C_SOURCE=200809L -fPIC -ftrapv -fvisibility=hidden -flto -fno-fat-lto-objects -Wl,-flto
LDFLAGS=-Wl,-rpath '-Wl,$$ORIGIN'
# MAKEFLAGS := --jobs=$(shell nproc) --output-sync=target
CWARN=-Wall -Wextra -Wno-format
  # -Wpedantic -Wsign-conversion -Wtype-limits -Wunused-result -Wnull-dereference \
	# -Waggregate-return -Walloc-zero -Walloca -Warith-conversion -Wcast-align -Wcast-align=strict \
	# -Wdangling-else -Wdate-time -Wdisabled-optimization -Wdouble-promotion -Wduplicated-branches \
	# -Wduplicated-cond -Wexpansion-to-defined -Wfloat-conversion -Wfloat-equal -Wformat-nonliteral \
	# -Wformat-security -Wformat-signedness -Wframe-address -Winline -Winvalid-pch -Wjump-misses-init \
	# -Wlogical-op -Wlong-long -Wmissing-format-attribute -Wmissing-include-dirs -Wmissing-noreturn \
	# -Wnull-dereference -Woverlength-strings -Wpacked -Wpacked-not-aligned -Wpointer-arith \
	# -Wredundant-decls -Wshadow -Wshadow=compatible-local -Wshadow=global -Wshadow=local \
	# -Wsign-conversion -Wstack-protector -Wsuggest-attribute=const -Wswitch-default -Wswitch-enum \
	# -Wsync-nand -Wtrampolines -Wundef -Wunsuffixed-float-constants -Wunused -Wunused-but-set-variable \
	# -Wunused-const-variable -Wunused-local-typedefs -Wunused-macros -Wvariadic-macros -Wvector-operation-performance \
	# -Wvla -Wwrite-strings
OSFLAGS != case $$(uname -s) in *BSD|Darwin) echo '-D_BSD_SOURCE';; Linux) echo '-D_GNU_SOURCE';; *) echo '-D_DEFAULT_SOURCE';; esac
EXTRA=
G=-ggdb
O=-Og
LIBS=-lgc -lgccjit -lcord -lm -lunistring -L. -l:libsss.so.$(VERSION)
ALL_FLAGS=$(CFLAGS) $(EXTRA) $(CWARN) $(G) $(O) $(OSFLAGS) -DSSS_VERSION=\"$(VERSION)\"

LIBFILE=libsss.so.$(VERSION)
CFILES=files.c parse.c ast.c environment.c args.c types.c typecheck.c compile/math.c compile/blocks.c compile/expr.c \
			 compile/functions.c compile/helpers.c compile/arrays.c compile/tables.c compile/loops.c compile/program.c compile/ranges.c \
			 compile/match.c compile/types.c util.c SipHash/halfsiphash.c
HFILES=files.h parse.h ast.h environment.h types.h typecheck.h compile/compile.h compile/libgccjit_abbrev.h util.h \
			 builtins/array.h builtins/functions.h builtins/range.h builtins/string.h builtins/table.h builtins/types.h
OBJFILES=$(CFILES:.c=.o)

BUILTIN_CFILES=builtins/integers.c builtins/floats.c builtins/char.c builtins/string.c builtins/bool.c builtins/range.c builtins/memory.c \
							 builtins/functions.c builtins/array.c builtins/table.c builtins/types.c
BUILTIN_OBJFILES=$(BUILTIN_CFILES:.c=.o)

all: $(LIBFILE) sss sss.1

$(LIBFILE): $(BUILTIN_OBJFILES) builtins/builtins.o SipHash/halfsiphash.o
	$(CC) $^ $(CFLAGS) $(EXTRA) $(CWARN) $(G) $(O) $(OSFLAGS) -lm -lgc -Wl,-soname,$(LIBFILE) -shared -o $@

sss: $(OBJFILES) $(BUILTIN_OBJFILES) $(HFILES) $(LIBFILE) sss.c
	$(CC) $(ALL_FLAGS) $(LIBS) $(LDFLAGS) -o $@ $(OBJFILES) $(BUILTIN_OBJFILES) sss.c

SipHash/halfsiphash.c:
	git submodule update --init --recursive

%.o: %.c $(HFILES)
	$(CC) -c $(ALL_FLAGS) -o $@ $<

%: %.c $(HFILES)
	$(CC) $(OSFLAGS) $(ALL_FLAGS) $(LIBS) $(LDFLAGS) -o $@ $^

tags: $(CFILES) $(BUILTIN_CFILES) $(HFILES) sss.c
	ctags $^

clean:
	rm -f sss $(OBJFILES) $(BUILTIN_OBJFILES) sss[0-9]+* libsss.so.*

sss.1: sss.1.md
	pandoc --lua-filter=.pandoc/bold-code.lua -s $< -t man -o $@

install: sss $(LIBFILE)
	mkdir -p -m 755 "$(PREFIX)/man/man1" "$(PREFIX)/bin" "$(PREFIX)/lib" "$(PREFIX)/share/sss/modules"
	cp -v sss.1 "$(PREFIX)/man/man1/sss.1"
	cp $(LIBFILE) "$(PREFIX)/lib/$(LIBFILE)"
	cp -vr stdlib/* "$(PREFIX)/share/sss/modules/"
	rm -f "$(PREFIX)/bin/sss" "$(PREFIX)/bin/sss$(VERSION)"
	cp -v sss "$(PREFIX)/bin/sss$(VERSION)"
	cp -v sss_version_picker.sh "$(PREFIX)/bin/sss"

uninstall:
	@rm -rvf "$(PREFIX)/bin/sss$(VERSION)" "$(PREFIX)/lib/$(LIBFILE)" "$(PREFIX)/share/sss"; \
	if [[ "`find "$(PREFIX)/bin" -type f -regex '.*/sss[0-9.]+\$$'`" == "" ]]; then \
		rm -vf "$(PREFIX)/man/man1/sss.1" "$(PREFIX)/bin/sss"; \
	else \
		if [ -f "$(PREFIX)/bin/sss" ]; then \
			read -p $$'\033[1mIt looks like there are other versions of sss installed. Do you want to uninstall them as well? [Y/n]\033[0m ' ans; \
			if [[ $$ans =~ ^[Yy] ]]; then \
				rm -vf "$(PREFIX)/bin"/sss* "$(PREFIX)"/lib/libsss.so* "$(PREFIX)"/man/man1/sss.1 ; \
			fi; \
		fi; \
	fi;

test: all
	@for f in test/*.sss stdlib/*.sss; do printf '\x1b[33;1;4m%s\x1b[m\n' "$$f" && ./sss $$f && printf '\x1b[32;1mPassed!\x1b[m\n\n' || exit 1; done
	@printf '\x1b[42;30m All tests passed! \x1b[m\n\n'

.PHONY: all clean install uninstall test
