include ~/.Renviron

all: pkg
	R_LIBS=$(R_LIBS) R CMD check Rapophenia
	R_LIBS=$(R_LIBS) R CMD build Rapophenia
	R_LIBS=$(R_LIBS) R CMD INSTALL -l ~/.Rlibs Rapophenia*.tar.gz
	#rm -f Rapophenia*.tar.gz

nocheck: pkg
	R_LIBS=$(R_LIBS) R CMD build Rapophenia
	R_LIBS=$(R_LIBS) R CMD INSTALL -l ~/.Rlibs Rapophenia*.tar.gz


#doesn't rebuild the pkg, so:
#cp what_i_changed Rapophenia/...
#make quick
quick: 
	R_LIBS=$(R_LIBS) R CMD build Rapophenia
	R_LIBS=$(R_LIBS) R CMD INSTALL -l ~/.Rlibs Rapophenia*.tar.gz

pkg: doc
	mkdir -p Rapophenia/inst/doc
	cp -r man R src tests Rapophenia
	cp -r pkging/* Rapophenia
	cp doc/rapop*.pdf  Rapophenia/inst/doc/
	cp src/latex/refman.pdf  Rapophenia/inst/doc/Rapohenia.pdf
	cd Rapophenia; autoconf; rm configure.ac; rm -r autom4te.cache

doc: 
	#cd src; doxygen doxyconfig
	cd doc; pdflatex rapophenia-guide.pdf
