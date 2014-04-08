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
	mkdir -p Rapophenia/inst/include
	cp -r man R src tests Rapophenia
	cp -r pkging/* Rapophenia
	cp -r Rapophenia/src/*.h Rapophenia/inst/include/
	-cp doc/rapop*.pdf  Rapophenia/inst/doc/
	-cp src/latex/refman.pdf  Rapophenia/inst/doc/Rapohenia.pdf
	cd Rapophenia; autoconf; rm configure.ac; rm -r autom4te.cache

doc: 
	#cd src; doxygen doxyconfig
	cd doc; pdflatex rapophenia-guide
	
HASH="`git log -1 | grep commit | cut -f2 -d' ' | head -c 8`"

push-pkg:
	git checkout -b pkg-$(HASH)
	make
	for i in `git ls-files`; do git rm $$i; done
	rsync -aP Rapophenia/ .
	git add .
	git rm -f Rapophenia*tar.gz config.log config.status
	git commit -a -m 'Rebuilt package'
	git merge -X theirs remotes/origin/pkg
	git push origin pkg
	git branch -d `git branch| grep pkg-`
	git checkout master
