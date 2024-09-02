.PHONY: all test clean

fmt:
	find . -name '*.hs' -type f -exec ormolu --mode inplace {} \;
	find . -name '*.nix' -exec nixfmt {} \;
	-statix check
	-deadnix -f

build: fmt
	nix --extra-experimental-features 'nix-command flakes' --accept-flake-config build .#
run: fmt
	nix --extra-experimental-features 'nix-command flakes' --accept-flake-config run .#

install-to-jjba: run
	cp -fv output.html ../jjba/projects/cv/index.html

