# Currently, GNU make is the default on our Macs, which do not
# support PMC counters, so just supply dummy build rules.

EBIN ?= ebin

NIF_LIB ?= $(EBIN)/pmc_nif.so

all:	$(NIF_LIB)

$(NIF_LIB): 
	@echo "Not building PMC library with GNU make"

clean:
	rm -f $(EBIN)/*.o $(NIF_LIB)
