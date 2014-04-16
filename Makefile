SRCS = \
	mpc.c \
	ore.c

OBJS = $(subst .c,.o,$(SRCS))

CFLAGS = -g
LIBS = -lm
TARGET = ore
ifeq ($(OS),Windows_NT)
TARGET := ${TARGET}.exe
endif
ifeq (yes, $(ORE_DEBUG))
CFLAGS := ${CFLAGS} -DDEBUG
endif

all : $(TARGET)

$(TARGET) : $(OBJS) mpc.h khash.h klist.h
	gcc -o $@ $(OBJS) $(LIBS)

.c.o :
	gcc -c $(CFLAGS) -I. $< -o $@

clean :
	rm -f *.o $(TARGET)

test :
	./t/test.sh
