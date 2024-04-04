.PHONY: default tests

default: tests

tests:
	@echo ""
	@echo "Running indent-tests.el"
	emacs --no-init-file --no-site-file --script tests/indent-tests.el
	@echo ""
	@for test in tests/*-test.sv ; do \
		echo "Checking $$test.golden, $$test.result"; \
		diff $$test.golden $$test.result --color \
			--ignore-trailing-space; \
		if [ $$? -ne 0 ] ; then exit 1; fi; \
	done
	@echo "All $$(ls -1q tests/*-test.sv | wc -l) tests passed."
	@echo ""
	@rm tests/*-test.sv.result
