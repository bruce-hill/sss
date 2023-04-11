NAME=blang
CC=cc
PREFIX=/usr/local
CFLAGS=-std=c11 -Werror -D_XOPEN_SOURCE=700 -D_POSIX_C_SOURCE=200809L -fPIC -ftrapv
LDFLAGS=-Wl,-rpath '-Wl,$$ORIGIN'
CWARN=-Wall -Wextra
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
O=-O0
LIBS=-lgc -lgccjit -lcord -lm -L. -lblang
ALL_FLAGS=$(CFLAGS) $(EXTRA) $(CWARN) $(G) $(O) $(OSFLAGS) $(LIBS)

LIBFILE=lib$(NAME).so
CFILES=span.c files.c parse.c ast.c environment.c types.c typecheck.c units.c compile/math.c compile/blocks.c compile/expr.c compile/functions.c compile/helpers.c compile/arrays.c compile/tables.c compile/loops.c compile/program.c compile/ranges.c util.c libblang/list.c libblang/utils.c libblang/string.c libblang/hashmap.c SipHash/halfsiphash.c
HFILES=span.h files.h parse.h ast.h environment.h types.h typecheck.h units.h compile/compile.h util.h libblang/list.h libblang/string.h libblang/hashmap.h
OBJFILES=$(CFILES:.c=.o)

all: blang blangc blang.1

$(LIBFILE): libblang/list.o libblang/utils.o libblang/string.o libblang/hashmap.o SipHash/halfsiphash.o
	$(CC) $^ $(CFLAGS) $(EXTRA) $(CWARN) $(G) $(O) $(OSFLAGS) -lgc -Wl,-soname,$(LIBFILE) -shared -o $@

blang: $(OBJFILES) $(HFILES) $(LIBFILE) blang.c
	$(CC) $(ALL_FLAGS) $(LDFLAGS) -o $@ $(OBJFILES) blang.c

hashtest: libblang/hashmap.o hashtest.c
	$(CC) $(ALL_FLAGS) $(LDFLAGS) -o $@ $^

blangc:
	ln -sv -T blang blangc

%.o: %.c $(HFILES)
	$(CC) -c $(ALL_FLAGS) -o $@ $<

%: %.c $(HFILES)
	$(CC) $(OSFLAGS) $(ALL_FLAGS) $(LDFLAGS) -o $@ $^

tags: $(CFILES) $(HFILES) blang.c
	ctags $^

clean:
	rm -f $(NAME) $(OBJFILES) $(LIBFILE)

blang.1: blang.1.md
	pandoc --lua-filter=.pandoc/bold-code.lua -s $< -t man -o $@

install: blang $(LIBFILE)
	mkdir -p -m 755 "$(PREFIX)/man/man1" "$(PREFIX)/bin" "$(PREFIX)/lib" "$(PREFIX)/share/blang/modules"
	cp blang.1 "$(PREFIX)/man/man1/$(NAME).1"
	cp $(LIBFILE) "$(PREFIX)/lib/$(LIBFILE)"
	cp -r stdlib/* "$(PREFIX)/share/blang/modules/"
	rm -f "$(PREFIX)/bin/$(NAME)"
	cp $(NAME) "$(PREFIX)/bin/"

uninstall:
	rm -rf "$(PREFIX)/bin/$(NAME)" "$(PREFIX)/man/man1/$(NAME).1" "$(PREFIX)/lib/$(LIBFILE)" "$(PREFIX)/share/blang"

test: all
	@for f in test/*.bl; do printf '\x1b[33;1;4m%s\x1b[m\n' "$$f" && ./blang $$f && printf '\x1b[32;1mPassed!\x1b[m\n\n' || exit 1; done
	@printf '\x1b[42;30m All tests passed! \x1b[m\n\n'

.PHONY: all clean install uninstall test
