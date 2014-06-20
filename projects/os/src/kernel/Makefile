CFLAGS=-c -m32 -ffreestanding -fno-stack-protector
LFLAGS=--oformat=binary -melf_i386 -T script.ld

all: startup.o stdlib.o main.o tty.o script.ld interrupts.o memory_manager.o 
	ld $(LFLAGS) -o kernel.bin startup.o stdlib.o main.o tty.o interrupts.o memory_manager.o 
	
startup.o: startup.i386.asm
	../fasm startup.i386.asm startup.o
	
stdlib.o: stdlib.c stdlib.h
	gcc $(CFLAGS) -o stdlib.o stdlib.c
main.o: main.c stdlib.h
	gcc $(CFLAGS) -o main.o main.c
memory_manager.o: memory_manager.c memory_manager.h stdlib.h
	gcc -c $(CFLAGS) -o memory_manager.o memory_manager.c
interrupts: interrupts.c interrupts.h stdlib.h
	gcc -c: $(CFLAGS) -o interrupts.o interrupts.c
tty.o: tty.c tty.h stdlib.h
	gcc -c $(CFLAGS) -o tty.o tty.c
	
clean:
	rm -v *.o kernel.bin 
