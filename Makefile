SRCS = \
	mpc.c \
	vm.c

OBJS = $(subst .c,.o,$(SRCS))

CFLAGS = 
LIBS = 
TARGET = orelang.exe

all : $(TARGET)

$(TARGET) : $(OBJS) mpc.h khash.h klist.h
	gcc -o $@ $(OBJS) $(LIBS)

.c.o :
	gcc -c $(CFLAGS) -I. $< -o $@

clean :
	rm -f *.o $(TARGET)
