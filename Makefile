SRCS = \
	deps/mpc/mpc.c \
	deps/klib/kstring.c \
	slre.c \
	ore.c

OBJS = $(subst .c,.o,$(SRCS))

CFLAGS = -g -Wall -Werror -Ideps/mpc -Ideps/klib
LIBS = -lm
TARGET = ore
ifeq ($(OS),Windows_NT)
TARGET := ${TARGET}.exe
endif
ifeq (yes, $(ORE_DEBUG))
CFLAGS := ${CFLAGS} -DDEBUG
endif

all : $(TARGET)

$(TARGET) : $(OBJS) deps/mpc/mpc.h deps/klib/khash.h deps/klib/klist.h deps/klib/kstring.h slre.h ore.h
	gcc -o $@ $(OBJS) $(LIBS)

.c.o :
	gcc -c $(CFLAGS) -I. $< -o $@

clean :
	rm -f *.o deps/mpc/*.o deps/klib/*.o $(TARGET)

test :
	sh -c ./t/test.sh
