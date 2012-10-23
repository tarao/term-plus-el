SHELL = /bin/sh
EMACS = emacs
FILES = $(wildcard *.el)
PROFILER =
LIBS =

ELCFILES = $(FILES:.el=.elc)

.PHONY: all compile compile-batch clean emacs term terminal

all: compile
compile: $(ELCFILES)

.depend: $(FILES)
	@echo Compute dependencies
	@rm -f .depend
	@for f in $(FILES); do \
	    sed -n "s/^.*(require '\(term+[^ )]*\)).*$$/$${f}c: \1.elc/p" $$f >> .depend;\
	done

-include .depend

$(ELCFILES): %.elc: %.el
	$(EMACS) --batch -Q -L . $(LIBS) \
-f batch-byte-compile $<

clean:
	rm -f *~
	rm -f \#*\#
	rm -f *.elc
	rm -f .depend

emacs:
	$(EMACS) -Q -L . $(LIBS) \
--eval "(require 'term+)" \
--eval "(require 'xterm-256color)" \
--eval "(call-interactively 'term)" &

term: terminal
terminal:
	$(EMACS) -nw -Q -L . $(LIBS) \
--eval "(require 'term+)" \
--eval "(require 'xterm-256color)" \
--eval "(call-interactively 'term)"
