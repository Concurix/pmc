
ERLC ?= erlc

SYSTEM = $(shell uname)

ifeq ($(SYSTEM),FreeBSD)
HAVE_PMCS=true
else ifeq ($(SYSTEM),Concurix)
HAVE_PMCS=true
else
HAVE_PMCS=false
endif

ESRC ?= src
EBIN ?= ebin

NIF_LIB ?= $(EBIN)/pmc_nif.so

APP_FILE := pmc.app
APP_SRC  := $(APP_FILE).src
APP      := $(EBIN)/$(APP_FILE)

all:	$(EBIN) $(APP) $(NIF_LIB)

$(EBIN):
	mkdir -p $(EBIN)

$(EBIN)/pmc.o: $(ESRC)/pmc.c
ifeq ($(HAVE_PMCS),true)
	gcc -I/usr/local/lib/erlang/usr/include -fPIC -c $< -o $@
else
	@echo "Not building PMC library"
endif

$(NIF_LIB): $(EBIN)/pmc.o
ifeq ($(HAVE_PMCS),true)
	gcc -o $@ -fPIC -shared $< -lpmc
else
	@echo "Not building PMC library"
endif

$(APP): $(ESRC)/$(APP_SRC)
	cp $(ESRC)/$(APP_SRC) $@

clean:
	$(RM) $(EBIN)/*.beam $(EBIN)/*.o $(NIF_LIB) $(APP)

