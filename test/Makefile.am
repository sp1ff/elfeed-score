AM_ELCFLAGS = -L ${srcdir}/..

TESTS = elfeed-score-tests		\
	test-struct-serde               \
	test-serde			\
	test-scoring			\
	elfeed-score-test-tag-rules	\
	elfeed-score-log-tests		\
	elfeed-score-test-feed-rules    \
	elfeed-score-test-maintenance   \
        test-explanations               \
	test-stats

dist_noinst_LISP = elfeed-score-tests.el		\
		   test-struct-serde.el                 \
		   test-serde.el			\
		   test-scoring.el		        \
		   elfeed-score-test-tag-rules.el	\
		   elfeed-score-log-tests.el		\
		   elfeed-score-test-feed-rules.el      \
		   elfeed-score-test-maintenance.el     \
                   test-explanations.el                 \
		   test-stats.el

EXTRA_DIST = $(TESTS)
