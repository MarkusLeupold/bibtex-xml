ROOTDIR := $(PWD)

SRCDIR    := $(ROOTDIR)/src
MAIN      := $(SRCDIR)/Main.hs
BUILDDIR  := $(ROOTDIR)/build
ODIR      := $(BUILDDIR)
OUTPUTDIR := $(BUILDDIR)
BINDIR    := $(ROOTDIR)/bin
BINMAIN   := $(BINDIR)/bibtoxml


install-dependencies:
	cabal update && \
	cabal install xml MissingH

build: $(MAIN) install-dependencies
	cd $(SRCDIR) && \
	ghc --make -outputdir $(OUTPUTDIR) -odir $(ODIR) -o $(BINMAIN) $(MAIN)
