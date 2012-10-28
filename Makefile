CFLAGS = $(pkg-config guile-2.0 --cflags)
LIBS   = $(pkg-config guile-2.0 --libs)

CFLAGS += -Wall -Wextra -Werror
CFLAGS += -fPIC
CFLAGS += -g

LDFLAGS += -lpcre -lguile $(LIBS)
LDFLAGS += -g

all: libguile-pcre.so

install: all
	cp libguile-pcre.so ~/local/lib

libguile-pcre.so: guile-pcre.o
	$(LD) $(LDFLAGS) -shared -o $@ $^

guile-pcre.o: guile-pcre.c guile-pcre.h Makefile

clean:
	-rm -rf libguile-pcre.so *.o
