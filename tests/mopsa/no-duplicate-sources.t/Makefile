CC ?= gcc
AR ?= ar

.PHONY: clean

all: libcool.a libhot.a bar

clean:
	rm -f libcool.a libhot.a bar a.o b.o bar.o

a.o: a.c
	$(CC) a.c -D FOO -c

b.o: b.c
	$(CC) b.c -D FOO -c

libcool.a: a.o b.o
	$(AR) rcs $@ $^

libhot.a: a.o b.o
	$(AR) rcs $@ $^

bar: bar.o
	$(CC) bar.o -o $@

bar.o: a.c
	$(CC) a.c -D BAR -c -o $@
