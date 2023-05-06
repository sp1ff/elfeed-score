AM_ELCFLAGS = --eval '(require (quote bytecomp))' -L ${srcdir}/..

TESTS = elfeed-score-tests \
	test-feed-rules    \
	test-udf-rules     \
	test-logs          \
	test-maintenance   \
	test-struct-serde  \
	test-serde	   \
	test-scoring	   \
	test-tag-rules	   \
        test-explanations  \
	test-stats	   \
	smoke-test

dist_noinst_LISP = elfeed-score-tests.el \
		   test-feed-rules.el    \
		   test-udf-rules.el     \
		   test-logs.el		 \
		   test-maintenance.el   \
		   test-struct-serde.el  \
		   test-serde.el	 \
		   test-scoring.el	 \
		   test-tag-rules.el	 \
                   test-explanations.el  \
		   test-stats.el	 \
		   smoke-test.el         \
		   smoke-test-core.el    \
		   package-test.el	 \
		   package-test-341.el	 \
		   upgrade-test.el

EXTRA_DIST = $(TESTS) package-test package-test-341 upgrade-test

AM_TESTS_ENVIRONMENT = \
	ELFEED_SCORE_VERSION=@PACKAGE_VERSION@ \
	srcdir=$(srcdir) \
	builddir=$(builddir)

# These are the tests that depend on having the Emacs package
# available, so they're run during `make distcheck` rather than `make
# check`.
distcheck-hook:
	${srcdir}/package-test &&     \
	${srcdir}/package-test-341 && \
	${srcdir}/upgrade-test

DISTCLEANFILES = package-test.log package-test.trs package-test-341.log package-test-341.trs \
	upgrade-test.log upgrade-test.trs autotools-test.log autotools-test.trs
