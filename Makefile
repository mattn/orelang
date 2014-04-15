SRCS = \
	mpc.c \
	vm.c

OBJS = $(subst .c,.o,$(SRCS))

CFLAGS = 
LIBS = -lm
TARGET = orelang
ifeq ($(OS),Windows_NT)
TARGET := ${TARGET}.exe
endif

all : $(TARGET)

$(TARGET) : $(OBJS) mpc.h khash.h klist.h
	gcc -o $@ $(OBJS) $(LIBS)

.c.o :
	gcc -c $(CFLAGS) -I. $< -o $@

clean :
	rm -f *.o $(TARGET)
