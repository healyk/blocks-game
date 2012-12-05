# -*- makefile -*-

OS = $(shell uname -o)
GAMBIT_HOME = /local/Gambit-C

# Include gambit libs in the path under windows
PATH := $(PATH):$(GAMBIT_HOME)/lib

#
# Configuration
#
CC   = gcc
SCM  = $(GAMBIT_HOME)/bin/gsc

PROG_NAME = blocks

SRC_DIR = src
BLD_DIR = build
OBJ_DIR = build/obj
PKG_DIR = build/package

C_SRC_DIR = $(SRC_DIR)/c
SCM_SRC_DIR = $(SRC_DIR)/scm

DEFS = -DGLFW_DLL -DWIN32 \
	-DSCHEME_LIBRARY_LINKER=____20_$(PROG_NAME)___2e_scheme

#
# Compiler config
#
# Window only TODO - make platform agnostic
#
CFLAGS = $(DEFS) -O0 -Wall -std=c99 -g -I./contrib/include \
	-I$(GAMBIT_HOME)/include
LDFLAGS = -Wl,-Bdynamic -llibgambc -lglfwdll -lopengl32 -lglu32 -lmingw32 \
	-lws2_32 -lOpenAL32 -mwindows -L./contrib/lib \
	-L$(GAMBIT_HOME)/lib

# Note that paths here are from where the file is being *compiled*.  This is
# a quirk of gsc.
SCMFLAGS = -cc-options "-D___LIBRARY $(DEFS) -I../../src/c -I../../contrib/include" \
	-debug

#
# Big 'ol list of sources and objects.
#
C_SRCS = $(wildcard $(C_SRC_DIR)/soil/*.c) $(wildcard $(C_SRC_DIR)/*.c)
C_OBJS = $(patsubst $(C_SRC_DIR)/%.c, $(OBJ_DIR)/%.o, $(C_SRCS))

# List of scheme sources.  Order matters.
SCM_SRCS = $(addprefix $(SCM_SRC_DIR)/, \
	srfi-1.scm \
	util.scm \
	bindings.scm \
	piece.scm \
	gamestate.scm \
	ingame.scm \
	ui.scm \
	mylib.scm)

SCM_C_SRCS = $(patsubst $(SCM_SRC_DIR)/%.scm, $(BLD_DIR)/gen/%.scheme.c, \
	$(SCM_SRCS))
SCM_OBJS   = $(patsubst $(BLD_DIR)/gen/%.scheme.c, $(OBJ_DIR)/%.scheme.o, \
	$(SCM_C_SRCS))

OBJS = $(C_OBJS) $(SCM_OBJS) $(OBJ_DIR)/$(PROG_NAME)_.scheme.o

.SUFFIXES:
.SUFFIXES: .scm .c .o .h

all: $(BLD_DIR)/$(PROG_NAME)

prebuild:
	@mkdir -p $(BLD_DIR)
	@mkdir -p $(OBJ_DIR)
	@cp $(GAMBIT_HOME)/lib/libgambc.dll $(BLD_DIR)
	@echo "Building $(BLD_DIR)..."

$(BLD_DIR)/$(PROG_NAME): prebuild $(OBJS)
	@$(CC) -o $@ $(OBJS) $(LDFLAGS)
	@echo "LINK    $@"

#
# C source builds
#
$(OBJ_DIR)/%.o: $(C_SRC_DIR)/%.c
	@mkdir -p $(dir $@)
	@$(CC) -c $(CFLAGS) -o $@ $<
	@echo "CC      $<"

$(OBJ_DIR)/%.scheme.o: $(BLD_DIR)/gen/%.scheme.c
	@mkdir -p $(dir $@)
	@$(SCM) -obj $(SCMFLAGS) -o $@ $<
	@echo "CC      $<"

#
# SCM source builds
#
$(BLD_DIR)/gen/%.scheme.c: $(SCM_SRC_DIR)/%.scm
	@mkdir -p $(dir $@)
	@$(SCM) -c $(SCMFLAGS) -o $@ $<
	@echo "SCM->C  $< -> $@"

$(BLD_DIR)/gen/$(PROG_NAME)_.scheme.c: $(SCM_C_SRCS)
	@$(SCM) -o $@ -link $^
	@echo "Linkfile $@"

#
# Running, debugging and other utils
#
run:
	@echo "Running $(BLD_DIR)/$(PROG_NAME)"
	@PATH=$PATH:./contrib/lib $(BLD_DIR)/$(PROG_NAME)

gdb:
	PATH=$PATH:./contrib/lib/ gdb $(BLD_DIR)/$(PROG_NAME)

package: 
	mkdir -p $(PKG_DIR)
	cp ./contrib/lib/*.dll $(PKG_DIR)
	cp $(GAMBIT_HOME)/lib/libgambc.dll $(PKG_DIR)
	cp readme.md $(PKG_DIR)
	cp $(BLD_DIR)/$(PROG_NAME) $(PKG_DIR)
	cp -R data/ $(PKG_DIR)
	cd $(PKG_DIR)
	@echo All files prepared.  Zip directory $(PKG_DIR)

clean:
	rm -rf $(BLD_DIR) blocks.log scores.dat
