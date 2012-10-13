# BSD make is the default on our FreeBSD and Concurix servers,
# which support PMC counters, so this actually builds the library.

ERLC ?= erlc

SYSTEM != uname

.if $(SYSTEM) == FreeBSD2
HAVE_PMCS=true
.elif $(SYSTEM) == Concurix
HAVE_PMCS=true
.else
HAVE_PMCS=false
.endif

ESRC ?= src
EBIN ?= ebin

NIF_LIB ?= $(EBIN)/pmc_nif.so

all:	$(EBIN) $(NIF_LIB)

$(EBIN):
	mkdir -p $(EBIN)

$(EBIN)/pmc.o: $(ESRC)/pmc.c
.if $(HAVE_PMCS)
	gcc -I/usr/local/lib/erlang/usr/include -fPIC -c $(ESRC)/pmc.c -o $@
.else
	@echo "Not building PMC library"
.endif

$(NIF_LIB): $(EBIN)/pmc.o
.if $(HAVE_PMCS)
	gcc -o $@ -fPIC -shared $(EBIN)/pmc.o -lpmc
.else
	@echo "Not building PMC library"
.endif

clean:
	rm -f $(EBIN)/*.o $(NIF_LIB)

