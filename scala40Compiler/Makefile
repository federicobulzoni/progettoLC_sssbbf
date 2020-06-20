# Makefile Scala40Compiler
DEMO_ERR = testCompleto
DEMO_TAC = testCompleto
PATH_DEMO = test

all : Scala40Compiler

Scala40Compiler : Scala40Compiler.hs AbsGramm.hs AbsTAC.hs Color.hs Environment.hs ErrM.hs Errors.hs Lexer.hs Parser.hs PrintTAC.hs Printer.hs StaticAnalysis.hs ThreeAddressCode.hs Typed.hs
	ghc Scala40Compiler.hs

clean :
	-rm -f *.hi *.o *.log *.aux *.dvi

demo: $(DEMO_ERR) $(DEMO_TAC)

$(DEMO_ERR):
	@echo
	@echo "==== Programma con errori ===="
	@echo
	./Scala40Compiler $(PATH_DEMO)/sbagliati/$(@).scala


$(DEMO_TAC):
	@echo
	@echo "==== Programma corretto ===="
	@echo
	./Scala40Compiler $(PATH_DEMO)/corretti/$(@).scala