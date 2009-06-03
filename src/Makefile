include ../include/globals.mk

all: $(ERL_BEHAVIOUR_OBJECTS) $(ERL_OBJECTS)

doc: 
	@echo "generating docs for escribe"
	$(ERL_RUN) edoc files $(ERL_SOURCES) $(ERL_STOP)
	mkdir -p $(DOC_DIR)
	mv *.html *.png *.css $(DOC_DIR)

clean:
	rm -f $(CLEAN_TARGETS)
	rm -rf $(DOC_DIR)

test:
	@echo "No tests for src directory"
