
MENHIR=menhir
OCAMLC=ocamlc
OCAMLLEX=ocamllex

SOURCES = ast.ml parser.ml lexer.ml lexerPhase3.ml parserPhase3.ml main.ml 

OBJECTS = $(SOURCES:.ml=.cmo)

.PHONY: clean all 

all: parser

parser: ast.cmo parser.cmi parser.cmo lexer.cmo parserPhase3.cmo lexerPhase3.cmo main.cmo 
	$(OCAMLC) -o $@ $(OBJECTS)

%.cmo: %.ml
	$(OCAMLC) -c $< -o $@

%.cmi: %.mli
	$(OCAMLC) -c $< -o $@

%.ml %.mli: %.mly
	rm -f $(<:.mly=.conflicts)
	$(MENHIR) -v --infer $<

%.ml: %.mll
	$(OCAMLLEX) $<

parser.mly: ast.ml

lexer.mll: parser.ml

parserPhase3.mly : ast.ml

lexerPhase3.mll : parserPhase3.ml

clean:
	rm -fr parser.ml parser.mli lexer.ml *.cmo parser *.cmi *~ *.automaton *.conflicts

parser.cmo: ast.cmo parser.cmi
lexer.cmo: parser.cmo
main.cmo: parser.cmo lexer.cmo

parserPhase3.cmo : ast.cmo parserPhase3.cmi
lexerPhase3.cmo : parserPhase3.cmo
