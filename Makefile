
all: tags ellesmere
32bit: all
fast: merged
prof: all
small: merged
	strip ellesmere

-include *.d
-include modules.mak

CC := gcc
CFLAGS  += -D_GNU_SOURCE -c -Wall -Werror -Wmissing-declarations -g -std=gnu99 -fno-builtin-log -I. $(USER_CFLAGS)
LD := gcc
LDFLAGS += -g

small: CFLAGS    += -Os -DNDEBUG -g0
#http://gcc.gnu.org/ml/gcc-help/2003-08/msg00128.html    -- doesn't seem to help
#small: CFLAGS    += -Wl,-static -fdata-sections -ffunction-sections -Wl,--gc-sections
small: LEXFLAGS  += -Cem
fast:  CFLAGS    += -O3 -DNDEBUG -Wno-strict-overflow #-Winline
fast:  LEXFLAGS  += --Fast

prof:  CFLAGS    += -pg -fprofile-arcs
prof:  CFLAGS    += -DNDEBUG
#prof:  CFLAGS    += -O3
prof:  LDFLAGS   += -pg -fprofile-arcs
prof:  LEXFLAGS  += --Fast

32bit: CFLAGS += -m32
32bit: LDFLAGS += -m32

stripped: all
	strip ellesmere

Y_FILES     := $(wildcard *.y)
L_FILES     := $(wildcard *.l)
LEX_C_FILES := $(patsubst %.l,%.l.c,$(L_FILES)) $(patsubst %.y,%.y.c,$(Y_FILES))
GEN_C_FILES := $(LEX_C_FILES) _merged.c

# ALL_X_FILES is all files with an extension of .x
# X_FILES is all source files (not generated files) with an extension of .x without a "main" function
#
GEN_O_FILES := $(patsubst %.c,%.o,$(GEN_C_FILES))
LEX_O_FILES := $(patsubst %.c,%.o,$(LEX_C_FILES))
ALL_C_FILES := $(sort $(GEN_C_FILES) $(wildcard *.c))
T_C_FILES   := $(wildcard *.t.c)
C_FILES     := $(filter-out $(T_C_FILES) ellesmere.c $(GEN_C_FILES), $(ALL_C_FILES))
GEN_H_FILES := $(patsubst %.l,%.l.h,$(L_FILES)) $(patsubst %.y,%.y.h,$(Y_FILES))
H_FILES     := $(sort $(GEN_H_FILES) $(wildcard *.h))
O_FILES     := $(patsubst %.c,%.o,$(C_FILES))
ALL_O_FILES := $(patsubst %.c,%.o,$(ALL_C_FILES))
ALL_D_FILES := $(patsubst %.c,%.d,$(ALL_C_FILES))
ALL_I_FILES := $(patsubst %.c,%.i,$(ALL_C_FILES))
ALL_S_FILES := $(patsubst %.c,%.s,$(ALL_C_FILES))
T_FILES     := $(patsubst %.t.c,%.t,$(T_C_FILES))

_merged.o: CFLAGS += -DFUNC="static " -Wno-unused-function -Wno-missing-declarations
_merged.i: CFLAGS += -DFUNC="static " -Wno-unused-function -Wno-missing-declarations
_merged.s: CFLAGS += -DFUNC="static " -Wno-unused-function -Wno-missing-declarations
_merged.s: _merged.c

# ellesmere.c has the native actions in it, and we want to be able to call dladdr on them
ellesmere.o: CFLAGS += -Wno-missing-declarations

merged: _merged.o $(GEN_O_FILES)
	$(LD) $(LDFLAGS) $^ -o ellesmere -lfl -ldl

_merged.c: $(GEN_H_FILES)
	grep MERGE *.c | sort -t: -n -k3 | awk 'BEGIN{ FS=":" } { print "#include \""$$1"\" // "$$3 }' > $@

tags: $(C_FILES) $(H_FILES)
	ctags -R .

ellesmere: ellesmere.o $(O_FILES) $(LEX_O_FILES)
	$(LD) $(LDFLAGS) $^ -o $@ -lfl -ldl -Wl,--export-dynamic #-lefence

$(ALL_O_FILES): %.o: %.c
	$(CC) $(CFLAGS) $< -o $@ -MMD -MF $(@:.o=.d)

$(ALL_I_FILES): %.i: %.c
	$(CC) $(CFLAGS) -E $< -o $@

$(ALL_S_FILES): %.s: %.c
	$(CC) $(CFLAGS) -S $< -o $@


tokens.o: lex.l.h

# flex and bison commands should be run on the .c files, not the .h files
$(GEN_H_FILES): %.h: %.c

lex.l.c: CFLAGS += -Wno-conversion
lex.l.c: lex.l
	flex -o $@ --header-file=lex.l.h $<

memreport.txt: ellesmere
	./ellesmere < fib.el 7>&1 1>&2 | sort -k2 -nk3 | uniq -c | sort -rn > $@

gprof.txt: gmon.out
	gprof ellesmere > gprof.txt

gmon.out: prof
	./ellesmere < fib.el

oprof.txt: fast
	sudo opcontrol --reset
	sudo opcontrol --start --no-vmlinux
	./ellesmere < fib.el
	sudo opcontrol --stop
	opannotate --assembly --objdump-params "-Mintel" --include-file ./ellesmere > $@
	sort -rn $@ > sorted-$@

bitvector.t: bitvector.t.o bitvector.o $(bitvector_DEPS)
	$(LD) $(LDFLAGS) $^ -o $@ #-lefence

parser.t: CFLAGS += -DPARSER_T
parser.t: grammar.o parser.o array.o symbols.o memory.o file.o objects.o bitvector.o stack.o records.o options.o walk.o
	$(LD) $(LDFLAGS) $^ -o $@ #-lefence

sheppard.dot: parser.t
	parser.t > $@ 3> trace.txt

sheppard_gen.py: parser.t
	time parser.t > $@ 3> trace.txt 4> sheppard.dot 5> table.txt

records.t: records.t.o records.o $(records_DEPS)
	$(LD) $(LDFLAGS) $^ -o $@ #-lefence

objects.t: objects.t.o objects.o stack.o options.o walk.o $(objects_DEPS)
	$(LD) $(LDFLAGS) $^ -o $@ -lefence

%.pdf: %.dot
	#neato -Tpdf < $< > $@
	dot -Tpdf $< > $@

states.dot: parser.t
	./parser.t > states.dot 3> trace.txt

modules.mak: modules.dot dot2mak
	grep '\->' $< | grep -v "constraint=false" | sort | dot2mak > $@

includes.dot: Makefile
includes.dot: ${C_FILES} $(H_FILES)
	grep '^[:space:]*#include "' $^ | awk 'BEGIN{FS="[.:]|#include \""} {print $$1, $$(NF-1)}' | sort -u | awk 'BEGIN{ print "digraph \"#includes\" {" } {print $$1, "->", $$2} END{ print "}" }' > $@

deps: $(O_FILES) $(LEX_O_FILES)

FILES_TO_CLEAN := ellesmere tags
FILES_TO_CLEAN += $(GEN_C_FILES) $(GEN_H_FILES) $(ALL_O_FILES) $(ALL_I_FILES) $(ALL_D_FILES)
FILES_TO_CLEAN += memreport.txt gmon.out $(wildcard *.gcda) oprof.txt sorted-oprof.txt gprof.txt
FILES_TO_CLEAN += bitvector.t parser.t records.t objects.t states.dot states.pdf trace.txt
FILES_TO_CLEAN += modules.pdf modules.mak
FILES_TO_CLEAN += includes.pdf includes.dot

clean:
	rm -f $(FILES_TO_CLEAN)

SUFFIXES_TO_GLOB   := o d i t gcda
GITIGNORE_PATTERNS := $(patsubst %,\n*.%,$(SUFFIXES_TO_GLOB)) # These patterns start with \n to prevent bash from glob-expanding them
FILTEROUT_PATTERNS := $(patsubst %,\%.%,$(SUFFIXES_TO_GLOB))

# Turns out this isn't really a super idea
not.really.gitignore: Makefile
	echo "# THIS FILE IS AUTO-GENERATED -- see Makefile$(GITIGNORE_PATTERNS)" > $@
	for FILE in $(filter-out $(FILTEROUT_PATTERNS),$(FILES_TO_CLEAN)); do echo $$FILE; done >> $@

.PHONY: all merged memreport.txt deps
