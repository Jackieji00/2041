PROGRAMS = gtkcounter gtkmsg

all : $(PROGRAMS)

gtkcounter : gtkcounter.ml
	ocamlopt -I +lablgtk2 -o gtkcounter lablgtk.cmxa gtkInit.cmx gtkcounter.ml

gtkmsg : gtkmsg.ml
	ocamlopt -I +lablgtk2 -o gtkmsg lablgtk.cmxa gtkInit.cmx gtkmsg.ml

clean:
	rm -f *.cm[oix] $(PROGRAMS)

