include include/globals.mk

all:  
	@mkdir -p ebin
	@(cd src; $(MAKE))

boot:
	@(cd support; $(MAKE))

debug:
	@mkdir -p ebin
	@(cd src; DEBUG_INFO=1 $(MAKE))

test: all 
	@(cd t; $(MAKE));
	@(cd t; $(MAKE) test)

doc:
	@(cd src; $(MAKE) doc)

clean:
	rm -rf $(ERL_DB)
	@(cd src; $(MAKE) clean)
	@(cd t; $(MAKE) clean)
	@(cd support; $(MAKE) clean)

dist-src: 
	tar czvf $(LIBNAME)-$(VSN).tar.gz src/ ebin/ include/ support/ Makefile

