dist_lisp_LISP = elfeed-score.el elfeed-score-log.el \
	elfeed-score-maint.el elfeed-score-rules.el \
	elfeed-score-rule-stats.el \
	elfeed-score-scoring.el elfeed-score-serde.el \
	elfeed-score-pkg.el
EXTRA_DIST = README.org
CLEANFILES = .pkg-tmp/ elfeed-score-$(PACKAGE_VERSION).tar
SUBDIRS = doc test

package: elfeed-score-$(PACKAGE_VERSION).tar $(srcdir)/README.org

srclisp=$(dist_lisp_LISP:%.el=$(srcdir)/%.el)

elfeed-score-$(PACKAGE_VERSION).tar: $(srclisp) $(srcdir)/README.org
	make -C $(builddir)/doc info && \
	pushd $(srcdir)/doc && install-info --dir=$(abs_builddir)/dir elfeed-score.info && popd && \
	mkdir -p .pkg-tmp/elfeed-score-$(PACKAGE_VERSION)/ && \
	cp $(srclisp) .pkg-tmp/elfeed-score-$(PACKAGE_VERSION)/ && \
	cp $(srcdir)/README.org .pkg-tmp/elfeed-score-$(PACKAGE_VERSION)/ && \
	cp $(srcdir)/doc/elfeed-score.info .pkg-tmp/elfeed-score-$(PACKAGE_VERSION)/ && \
	mv $(abs_builddir)/dir .pkg-tmp/elfeed-score-$(PACKAGE_VERSION)/ && \
	cd .pkg-tmp && tar cf $@ elfeed-score-$(PACKAGE_VERSION)/ && \
	cd .. && mv -v .pkg-tmp/elfeed-score-$(PACKAGE_VERSION).tar . && \
	rm -rf .pkg-tmp

dist-hook: package

# I have mutliple tests that depend upon the elfeed-score Emacs
# package (i.e. elfeed-score-x.y.z.tar) being built, so I delay them
# until here (i.e. they won't be run on `make check`, but during `make
# distcheck`).

# NB. At this point, the source directory has been unpacked into
# `elfeed-score-x.y.z` and the distribution(s) are present in
# $(abs_top_builddir), but the other build directories have been
# cleaned at this point.
distcheck-hook:
	cd test && $(MAKE) \
	PACKAGE_VERSION=$(PACKAGE_VERSION) \
	abs_srcdir=$(abs_srcdir)/test \
	abs_top_builddir=$(abs_top_builddir) \
	distcheck-hook
