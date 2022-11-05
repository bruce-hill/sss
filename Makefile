NAME=blang
CC=cc
PREFIX=/usr/local
SYSCONFDIR=/etc
CFLAGS=-std=c99 -Werror -D_XOPEN_SOURCE=700 -D_POSIX_C_SOURCE=200809L -flto -fPIC
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
LIBS=-lgc -lbp -lgccjit -lcord -lbhash -lintern -lm -L. -lblang
ALL_FLAGS=$(CFLAGS) $(EXTRA) $(CWARN) $(G) $(O) $(OSFLAGS) $(LIBS)

LIBFILE=lib$(NAME).so
CFILES=ast.c parse.c types.c typecheck.c compile.c util.c libblang/list.c libblang/utils.c
HFILES=ast.h parse.h types.h typecheck.h compile.h util.h libblang/list.h
OBJFILES=$(CFILES:.c=.o)

all: blang blangc

$(LIBFILE): libblang/list.o libblang/utils.o
	$(CC) $^ $(CFLAGS) $(EXTRA) $(CWARN) $(G) $(O) $(OSFLAGS) -lgc -Wl,-soname,$(LIBFILE) -shared -o $@

blang: $(OBJFILES) $(HFILES) $(LIBFILE) blang.c
	$(CC) $(ALL_FLAGS) $(LDFLAGS) -o $@ $(OBJFILES) blang.c

blangc:
	ln -sv -T blang blangc

%.o: %.c $(HFILES)
	$(CC) -c $(ALL_FLAGS) -o $@ $<

tags: $(CFILES) blang.c
	ctags *.c *.h

clean:
	rm -f $(NAME) $(OBJFILES) $(LIBFILE)

.PHONY: all clean install uninstall test
