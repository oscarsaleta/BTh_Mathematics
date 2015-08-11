# Optimització
OPT=-g -Wall
#OPT=-O3 -Wall

# Tot:
all : testGPD

# Utilitats:
# - Test GPD
testGPD : testGPD.c brent.o
	gcc -o testGPD $(OPT) testGPD.c brent.o -lm

# Biblioteca
brent.o : brent.c 
	gcc -c $(OPT) brent.c -lm

# Neteja
clean :
	rm -f *.o
realclean : clean
	rm -f testGPD
