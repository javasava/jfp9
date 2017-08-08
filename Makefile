OCAMLC ?= ocamlopt
CFLAGS = -thread
CMAS = unix.cmxa threads.cmxa str.cmxa

all: epreuve1 epreuve3
clean:
	rm -rf mlgrope *.cmo *.cmi *.cmx *.o
.PHONY: all clean

%:
	$(OCAMLC) $(CFLAGS) -o $@ $(CMAS) game.mli movement.mli parser.mli command.mli game.ml movement.ml parser.ml command.ml $@.ml
tarball:
	tar czf submission.tar.gz *
mrproper: clean
	 rm -f epreuve1 epreuve3 submission.tar.gz *.out
