SCHEME = chezscheme --libdirs lib --program

check:
	$(SCHEME) test-primitives.sps
	$(SCHEME) test-threading.sps
	$(SCHEME) tests.sps

.PHONY: check
