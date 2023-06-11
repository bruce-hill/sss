CC=gcc
PREFIX=/usr/local
VERSION=0.8.4
CFLAGS=-std=c11 -Werror -D_XOPEN_SOURCE=700 -D_POSIX_C_SOURCE=200809L -fPIC -ftrapv
LDFLAGS=-Wl,-rpath '-Wl,$$ORIGIN'
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
CFILES=span.c files.c parse.c ast.c environment.c types.c typecheck.c units.c compile/math.c compile/blocks.c compile/expr.c \
			 compile/functions.c compile/helpers.c compile/arrays.c compile/tables.c compile/loops.c compile/program.c compile/ranges.c \
			 compile/match.c util.c libsss/list.c libsss/utils.c libsss/string.c libsss/hashmap.c libsss/base64.c SipHash/halfsiphash.c
HFILES=span.h files.h parse.h ast.h environment.h types.h typecheck.h units.h compile/compile.h util.h libsss/list.h libsss/string.h libsss/hashmap.h
OBJFILES=$(CFILES:.c=.o)

all: sss $(LIBFILE) sss.1

$(LIBFILE): libsss/list.o libsss/utils.o libsss/string.o libsss/hashmap.o libsss/base64.o SipHash/halfsiphash.o
	$(CC) $^ $(CFLAGS) $(EXTRA) $(CWARN) $(G) $(O) $(OSFLAGS) -lgc -Wl,-soname,$(LIBFILE) -fvisibility=hidden -shared -o $@

sss: $(OBJFILES) $(HFILES) $(LIBFILE) sss.c
	$(CC) $(ALL_FLAGS) $(LIBS) $(LDFLAGS) -o $@ $(OBJFILES) sss.c

%.o: %.c $(HFILES)
	$(CC) -c $(ALL_FLAGS) -o $@ $<

%: %.c $(HFILES)
	$(CC) $(OSFLAGS) $(ALL_FLAGS) $(LIBS) $(LDFLAGS) -o $@ $^

tags: $(CFILES) $(HFILES) sss.c
	ctags $^

clean:
	rm -f sss $(OBJFILES) sss[0-9]+* libsss.so.*

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
	@for f in test/*.sss; do printf '\x1b[33;1;4m%s\x1b[m\n' "$$f" && ./sss $$f && printf '\x1b[32;1mPassed!\x1b[m\n\n' || exit 1; done
	@printf '\x1b[42;30m All tests passed! \x1b[m\n\n'

.PHONY: all clean install uninstall test
