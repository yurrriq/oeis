.PHONY: docs
docs:
	cabal haddock --haddock-hyperlink-source --haddock-output-dir $@
