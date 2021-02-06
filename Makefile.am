dist_lisp_LISP = elfeed-score.el elfeed-score-log.el \
	elfeed-score-maint.el elfeed-score-rules.el \
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
