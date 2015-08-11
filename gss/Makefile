# Optimització
OPT=-g -Wall
#OPT=-O3 -Wall

# Tot:
all : test testMLE

# Utilitats:
# - Test polinomi
test : test.c gss.o gss.h eGDP.o eGDP.h
	gcc -o test $(OPT) test.c gss.o eGDP.o -lm
# - Test MLE
testMLE : testMLE.c gss.o gss.h eGDP.o eGDP.h
	gcc -o testMLE $(OPT) testMLE.c gss.o eGDP.o -lm

# Biblioteca
gss.o : gss.c 
	gcc -c $(OPT) gss.c -lm
eGDP.o : eGDP.c
	gcc -c $(OPT) eGDP.c -lm

# Neteja
clean :
	rm -f *.o
realclean : clean
	rm -f test testMLE
