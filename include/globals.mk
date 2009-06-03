INCLUDE_DIRS := ../include
SUPPORT_DIRS := ../include
DOC_DIR := ../docs
EBIN_DIR := ../ebin
EMULATOR := beam
VSN := 0.01.0
APPNAME := edownloader

ERLC_FLAGS := -W $(INCLUDE_DIRS:../%=-I ../%) $(EBIN_DIR:%=-pa %)

ifdef DEBUG_INFO
  ERLC_FLAGS += +debug_info
endif

ifdef DEBUG
  ERLC_FLAGS += -Ddebug
endif

ERL := erl
ERLC := $(ERL)c 

ERL_DB := Mnesia.nonode\@nohost

#erlang source files
ERL_SOURCES := $(wildcard *.erl)
ERL_BEHAVIOUR_SOURCES := $(wildcard behaviour/*.erl)
ERL_HEADERS := $(wildcard *.hrl) $(wildcard ../include/*.hrl)
EDOC_IMAGES := $(wildcard *.png) $(wildcard *.jpg)
EDOC_FILES := $(wildcard *.html) $(wildcard *.css) $(EDOC_IMAGES)
#expected object files from a compile
ERL_OBJECTS := $(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.$(EMULATOR))
ERL_BEHAVIOUR_OBJECTS := $(ERL_BEHAVIOUR_SOURCES:%.erl=$(EBIN_DIR)/%.$(EMULATOR))

#expected doc files to generate
ERL_DOCS := $(ERL_SOURCES:%.erl=$(DOC_DIR)/%.html)

#expected local objects from a compile
ERL_OBJECTS_LOCAL := $(ERL_SOURCES:%.erl=./%.$(EMULATOR))

#all the app files
APP_FILES := $(wildcard *.app)

#expected files in ebin
EBIN_FILES = $(ERL_OBJECTS) $(APP_FILES:%.app=../ebin/%.app)

#expected module names derived from source file names
MODULES = $(ERL_SOURCES:%.erl=%)

#common set of targets for cleaning
CLEAN_TARGETS := $(EBIN_FILES) $(ERL_OBJECTS_LOCAL) edoc-info erl_crash.dump escribe.log

ERL_RUN := $(ERL) -pa $(EBIN_DIR) -run 
ERL_STOP := -run init stop -noshell

#implicit rule to compile erl files to emulator objects
$(EBIN_DIR)/%.$(EMULATOR): %.erl
	@echo "Compiling $<"
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

default: all

clean_db:
	rm -rf ./$(ERL_DB) 

