current_dir := $(shell pwd)
ghc_version := $(shell ghc --version | head -n 1 | grep -oP '\d+\.\d+\.\d+')
project_version := ${shell grep -E '^version:' asciidoc-gen.cabal | grep -oP '\d+\.\d+\.\d+\.\d+'}

.PHONY: init
init:
	echo "Setting project-directory..."
	mkdir -p ~/.asciidoc-gen
	echo -n "${current_dir}/" > ~/.asciidoc-gen/project-directory
	sudo mkdir -p /root/.asciidoc-gen
	sudo cp ~/.asciidoc-gen/project-directory /root/.asciidoc-gen/project-directory

.PHONY: clean_install
clean_install:
	echo "Cleaning install..."
	find ~/.cabal/store/ghc-${ghc_version}/ -name 'asciidoc-gen-${project_version}*' -exec rm -rf {} +
	-rm ~/.cabal/bin/asciidoc-gen

.PHONY: install
install: clean_install
	echo "Installing..."
	cabal install

.PHONY: upgrade
upgrade:
	git fetch origin
	git reset --hard origin/master
	make install