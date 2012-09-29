
ERLC ?= erlc

EFLAGS += +debug_info +warn_exported_vars +warn_unused_vars +warn_unused_import +warn_missing_spec

ESRC ?= src
EBIN ?= ebin

MODULE ?= $(EBIN)/pmc.beam
NIF_LIB ?= $(EBIN)/pmc_nif.so

APP_FILE := pmc.app
APP_SRC  := $(APP_FILE).src
APP      := $(EBIN)/$(APP_FILE)

all:	$(EBIN) $(APP) $(MODULE) $(NIF_LIB)

$(EBIN):
	mkdir -p $(EBIN)

$(EBIN)/%.beam: $(ESRC)/%.erl
	@echo "    ERLC $<"
	@$(ERLC) $(EFLAGS) -o $(EBIN) $<

$(EBIN)/pmc.o: $(ESRC)/pmc.c
	gcc -I/usr/local/lib/erlang/usr/include -fPIC -c $< -o $@


$(NIF_LIB): $(EBIN)/pmc.o
	gcc -o $@ -fPIC -shared $< -lpmc

$(APP): $(ESRC)/$(APP_SRC)
	cp $(ESRC)/$(APP_SRC) $@

clean:
	$(RM) $(EBIN)/*.beam $(NIF_LIB) $(APP)




# all:	pmc_nif.so

# pmc_nif.so:	pmc.o
# 	gcc -o pmc_nif.so -fPIC -shared pmc.o -lpmc

# pmc.o:	pmc.c
# 	gcc -I/usr/local/lib/erlang/usr/include -fPIC -c pmc.c -o pmc.o

# clean:
# 	rm pmc.o pmc_nif.so

# pmc_test:	pmc_test.c
# 	gcc -I/usr/local/lib/erlang/usr/include pmc_test.c -o pmc_test -lpmc

