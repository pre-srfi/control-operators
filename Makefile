SCHEME = chezscheme --libdirs lib --program

check:
	$(SCHEME) test-primitives.sps
	$(SCHEME) test-control-operators.sps

.PHONY: check
