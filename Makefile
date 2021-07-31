SCHEME = chezscheme --libdirs lib --program

check:
	$(SCHEME) tests.sps

.PHONY: check
