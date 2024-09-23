.PHONY: all test clean

fmt:
	find . -name '*.hs' -type f -exec ormolu --mode inplace {} \;
	find . -name '*.nix' -exec nixfmt {} \;
	-statix check
	-deadnix -f

build: fmt
	stack build
run: fmt
	stack run

install-to-jjba: run
	cp -fv output.html ../jjba/projects/cv/index.html

