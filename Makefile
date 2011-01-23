
all: tags ellesmere
32bit: all
fast: merged
prof: merged
small: merged
	strip ellesmere

-include *.d

CC := gcc
CFLAGS  += -c -Wall -Werror -Wmissing-declarations -g -std=gnu99 -I.
LD := gcc
LDFLAGS += -g

small: CFLAGS    += -Os -DNDEBUG -g0
small: LEXFLAGS  += -Cem
fast:  CFLAGS    += -O3 -DNDEBUG -Winline
fast:  LEXFLAGS  += --Fast
prof:  CFLAGS    += -pg -O3 -DNDEBUG -fprofile-arcs
prof:  LDFLAGS   += -pg -fprofile-arcs
prof:  LEXFLAGS  += --Fast

32bit: CFLAGS += -m32
32bit: LDFLAGS += -m32

stripped: all
	strip ellesmere

Y_FILES     := $(wildcard *.y)
L_FILES     := $(wildcard *.l)
GEN_C_FILES := $(patsubst %.l,%.l.c,$(L_FILES)) $(patsubst %.y,%.y.c,$(Y_FILES))

small: GEN_C_FILES += _merged.c
fast:  GEN_C_FILES += _merged.c
prof:  GEN_C_FILES += _merged.c

GEN_O_FILES := $(patsubst %.c,%.o,$(GEN_C_FILES))
C_FILES     := $(sort $(GEN_C_FILES) $(wildcard *.c))
GEN_H_FILES := $(patsubst %.l,%.l.h,$(L_FILES)) $(patsubst %.y,%.y.h,$(Y_FILES))
H_FILES     := $(sort $(GEN_H_FILES) $(wildcard *.h))
O_FILES     := $(patsubst %.c,%.o,$(C_FILES))
D_FILES     := $(patsubst %.c,%.d,$(C_FILES))
I_FILES     := $(patsubst %.c,%.i,$(C_FILES))
S_FILES     := $(patsubst %.c,%.s,$(C_FILES))

_merged.o: CFLAGS += -DFUNC="static " -Wno-unused-function
_merged.i: CFLAGS += -DFUNC="static " -Wno-unused-function
_merged.s: CFLAGS += -DFUNC="static " -Wno-unused-function
_merged.s: _merged.c

merged: _merged.o $(GEN_O_FILES)
	$(LD) $(LDFLAGS) $^ -o ellesmere -lfl

_merged.c: $(GEN_H_FILES)
	grep MERGE *.c | sort -t: -n -k3 | awk 'BEGIN{ FS=":" } { print "#include \""$$1"\" // "$$3 }' > $@

tags: $(C_FILES) $(H_FILES)
	ctags -R .

ellesmere: $(O_FILES)
	$(LD) $(LDFLAGS) $^ -o $@ -lfl #-lefence

$(O_FILES): %.o: %.c
	$(CC) $(CFLAGS) $< -o $@ -MMD -MF $(@:.o=.d)

$(I_FILES): %.i: %.c
	$(CC) $(CFLAGS) -E $< -o $@

$(S_FILES) _merged.s: %.s: %.c
	$(CC) $(CFLAGS) -S $< -o $@


ellesmere.o: lex.l.h

# flex and bison commands should be run on the .c files, not the .h files
$(GEN_H_FILES): %.h: %.c

lex.l.c: lex.l
	flex -o $@ --header-file=lex.l.h $<

memreport.txt: ellesmere
	./ellesmere < fib.el 6>&1 1>&2 | sort -k2 -nk3 | uniq -c | sort -rn > $@

gprof.txt: gmon.out
	gprof ellesmere > gprof.txt

gmon.out: prof
	./ellesmere < fib.el

bitvector.t: CFLAGS += -DBITVECTOR_T
bitvector.t: bitvector.o memory.o file.o
	$(LD) $(LDFLAGS) $^ -o $@ #-lefence

parser.t: CFLAGS += -DPARSER_T
parser.t: grammar.o parser.o array.o symbols.o memory.o file.o objects.o bitvector.o stack.o
	$(LD) $(LDFLAGS) $^ -o $@ #-lefence

%.pdf: %.dot
	#neato -Tpdf < $< > $@
	dot -Tpdf < $< > $@

states.dot: parser.t
	./parser.t > states.dot 3> trace.txt

clean:
	rm -f memreport.txt ellesmere tags _merged.c gmon.out *.gcda $(GEN_C_FILES) $(GEN_H_FILES) $(O_FILES) $(I_FILES) $(D_FILES)
	rm -f bitvector.t parser.t states.dot states.pdf trace.txt

.PHONY: all merged memreport.txt
