# example of Makefile for Scheme+ for Guile
# Makefile for λογικι program
# author: Damien Mattei

# tree structure directories:
# .
# Makefile
# guile/
# guile/module_directory



# program to build (note: not used, will be recompiled by Guile if necessary when loaded) 
MYPROGRAM=start-λογικι-guile+.scm

# path to Scheme+ for Guile directory
SCHEME_PLUS_FOR_GUILE=../Scheme-PLUS-for-Guile

SRC_DIR=guile

# Scheme+ for Guile parser
PARSER:=$(SCHEME_PLUS_FOR_GUILE)/curly-infix2prefix4guile.scm


# sub directory where parsed module files will be generated
MODULE_DIRECTORY=$(SRC_DIR)/module_directory

# Guile modules to build
MODULES_NAME=minterms+.scm operation+.scm regex+.scm set+.scm subscript+.scm
MODULES=$(addprefix $(MODULE_DIRECTORY)/,$(MODULES_NAME))

# files that are simply included in source code
INCLUDED_FILES=$(SRC_DIR)/logiki-.scm

# files to parse
OBJECT= $(MODULES) $(INCLUDED_FILES)

# find the system directory for Guile modules
SITE_DIR=$(shell guile -c '(begin (display (%site-dir)) (newline))')


# create directory, build objects
# note: object modules have the same name than module source but are in different directories
all: $(MODULE_DIRECTORY) $(OBJECT) 


# create the sub directory where parsed module files will be
$(MODULE_DIRECTORY) :
	mkdir $@


# create Scheme files (*.scm and *-.scm) by parsing Scheme+ files (*+.scm)
$(MODULE_DIRECTORY)/%+.scm $(SRC_DIR)/%-.scm: $(SRC_DIR)/%+.scm
	@echo PARSING $< :
	$(PARSER) $< > $@


clean:
	rm -rf $(OBJECT)
	rm -rf $(MODULE_DIRECTORY)


install:
	cp $(MODULES) $(SITE_DIR)

