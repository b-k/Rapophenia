include ~/.Renviron

all: pkg
	R_LIBS=$(R_LIBS) R CMD check Rapophenia
	R_LIBS=$(R_LIBS) R CMD build Rapophenia
	R_LIBS=$(R_LIBS) R CMD INSTALL -l ~/.Rlibs Rapophenia*.tar.gz
	#rm -f Rapophenia*.tar.gz

nocheck: pkg
	R_LIBS=$(R_LIBS) R CMD build Rapophenia
	R_LIBS=$(R_LIBS) R CMD INSTALL -l ~/.Rlibs Rapophenia*.tar.gz

pkg: doc
	mkdir -p Rapophenia/inst/doc
	cp -r man R src tests Rapophenia
	cp -r pkging/* Rapophenia
	cp src/latex/refman.pdf  Rapophenia/inst/doc/Rapohenia.pdf
	cd Rapophenia; autoconf; rm configure.ac; rm -r autom4te.cache

doc: 
	cd src; doxygen doxyconfig
	cd src/latex; make
