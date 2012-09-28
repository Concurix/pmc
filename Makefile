
all:	pmc_nif.so

pmc_nif.so:	pmc.o
	gcc -o pmc_nif.so -fPIC -shared pmc.o -lpmc

pmc.o:	pmc.c
	gcc -I/usr/local/lib/erlang/usr/include -fPIC -c pmc.c -o pmc.o

clean:
	rm pmc.o pmc_nif.so

pmc_test:	pmc_test.c
	gcc -I/usr/local/lib/erlang/usr/include pmc_test.c -o pmc_test -lpmc

