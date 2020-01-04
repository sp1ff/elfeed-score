[![img](https://melpa.org/packages/elfeed-score-badge.svg)](https://melpa.org/#/elfeed-score)
[![img](https://stable.melpa.org/packages/elfeed-score-badge.svg)](https://stable.melpa.org/#/elfeed-score)

# Introduction

[elfeed-score](https://github.com/sp1ff/elfeed-score) brings Gnus-style [scoring](https://www.gnu.org/software/emacs/manual/html_node/gnus/Scoring.html#Scoring) to [Elfeed](https://github.com/skeeto/elfeed).

[Elfeed](https://github.com/skeeto/elfeed) is an extensible web feed reader for Emacs. By default, it will display entries in reverse chronological order. This package defines a bit of metadata for each of your feed entries: a "score". A score is an integer (negative or positive), and higher scores denote entries of greater interest to you. This package also installs a new sort function, so that Elfeed will display entries with higher scores before entries with lower scores (entries with the same scores will still be sorted in reverse chronological order).

While you can manually assign a score to an entry, you will likely find it more convenient to create rules for scoring that will be automatically applied to each new entry every time you update Elfeed. You can currently score against title, feed & content by defining strings that will be matched against those attributes by either substring or regexp match. Each rule defines an integral value, and the rules are applied in order of definition. The new entry's score begins at zero, and is adjusted by the value defined by each matching scoring rule.

For instance, here's a subset of my scoring file at the moment:

    ;;; Elfeed score file                                     -*- lisp -*-
    (("title"
      ("OPEN THREAD" -1000 S 1576681345.4086394)
      ("china" 150 s 1576808786.1848788)
      ("california" 150 s 1576808786.1203141)
      ("raymond c\\(hen\\)?" 250 r 1576808786.1558545))
     ("content"
      ("california" 100 s 1576808786.4068587)
      ("china" 100 s 1576808786.4004376)
      ("type erasure" 500 s 1576808786.043517))
     ("feed"
      ("Essays in Idleness" 250 S t 1576808786.1956885)
      ("Irreal" 250 S t 1576808786.1765869)
      ("Julia Evans" 100 s t 1576808786.4092398)
      ("National Weather Service" 400 S t 1576808786.1117532)
      ("emacs-news – sacha chua" 350 S t 1576808785.3807983))
     (mark -2500))

Like Gnus scoring, this may look like Lisp code, but it is not directly eval'd. It will be read by the Lisp reader, so it must at least be a valid Lisp s-expression. You can find details below.

# Prerequisites

This package was developed against [Elfeed](https://github.com/skeeto/elfeed) 3.3.0, which itself requires Emacs 24.3 and cURL.

# Installing

The easiest way to install elfeed-score is [MELPA](https://github.com/melpa/melpa); assuming you've got MELPA in your ='package-archives=, just say:

    (use-package elfeed-score
      :ensure t
      :config
      (progn
        (elfeed-score-enable)
        (define-key elfeed-search-mode-map "=" elfeed-score-map)))

You can, if you wish, install from source as well:

    cd /tmp
    curl -L --output=elfeed-score-0.2.0.tar.gz https://github.com/sp1ff/elfeed-score/archive/0.2.0.tar.gz
    tar xvf elfeed-score-0.2.0.tar.gz && cd elfeed-score-0.2.0
    ./configure
    make
    make install

## Running the Unit Tests

The unit tests require some macros defined by the [Elfeed](https://github.com/skeeto/elfeed) test suite, which is not distributed with the MELPA package. Therefore, you'll need to clone the Elfeed git repo & develop against that:

    cd /tmp
    git clone https://github.com/skeeto/elfeed.git
    curl -L --output=elfeed-score-0.2.0.tar.gz https://github.com/sp1ff/elfeed-score/archive/0.2.0.tar.gz
    tar xvf elfeed-score-0.2.0.tar.gz && cd elfeed-score-0.2.0
    export EMACSLOADPATH=/tmp/elfeed-score-0.2.0:/tmp/elfeed/tests:$EMACSLOADPATH
    ./configure
    make
    make check

Unless you already use `EMACSLOADPATH`, this likely won't work as written&#x2013; you'll need to work out exactly how to tell Emacs to pickup Elfeed from your git repo instead of wherever you've got it installed. If you're running the unit tests, I assume you can get this working.

# Getting Started

## Score File Format

The score file (`~/.emacs.d/elfeed.score` by default) is an Emacs Lisp form that evaluates to an association list. Comments are as per usual. The current format recognizes four keys:

-   "title": the value associated with this is a list of rules matching text against the entry title
-   "content": the value associated with this is a list of rules matching text against the entry content
-   "feed": the value associated with this is a list of rules matching text against the entry feed
-   `mark`: an integer which, if greater than an entry's final score, will result in the entry being marked as read

Title & content rules are defined by a list of length four:

1.  the match text
2.  the match value: this is an integer specifying the amount by which the entry's score should be adjusted, should the text match
3.  the match type: this may be one of `s`, `S`, `r` or `R` for substring match, case-sensitive substring match, regexp match or case-sensitive regexp match, respectively
4.  the last time this rule matched an entry, in seconds since Unix epoch. This element is optional, need not be supplied by the score file author, and will be automatically kept up-to-date by the package.

So, when first setting up your score file, saying:

    ;;; Elfeed score file                                     -*- lisp -*-
    (("title"
      ("OPEN THREAD" -1000 S))
     ("content"
      ("california" 100 s)))

means that you want all entries whose title contains the text "OPEN THREAD" to have its score decreased by 1000, and whose content contains the text "california" to have its score increased by 100. The former match will be case-sensitive, the latter case-insensitive.

Scoring against the entry's feed is done similarly, but may be done against either the feed title or the feed URL. This is indicated by adding a new element at index 3 which may be one of `t` or `u` (for title or URL, respectively).

Finally, if you've decided that an entry's score is low enough, you may not even want to see it. In that casse, add a rule like:

    (mark N)

when the entry's final score is below `N`, the package will remove the `unread` tag from the entry, marking it as "read".

## Using elfeed-score

Once your score file is setup, load elfeed-score. 

    (require 'elfeed-score)

Just loading the library will **not** modify [Elfeed](https://gitub.com/skeeto/elfeed); you need to explicitly enable the package for that:

    (elfeed-score-enable)

This will install the new sort function & new entry hook, as well as read your score file. NB. `elfeed-score-enable` is autoloaded, so if you've installed this package in the usual ways, you should be able to just invoke the function & have the package loaded & enabled automatically.

The package defines a keymap, but does not bind it to any key. I like to set it to the `=` key in `elfeed-search-mode-map`:

    (define-key elfeed-search-mode-map "=" elfeed-score-map)

At this point, any <span class="underline">new</span> entries will be scored automatically, but the entries already in your database have not yet been scored. Scoring is idempotent (scoring an entry more than once will always result in it having the same score assigned). So, you can load up an Elfeed search, and then, in the Elfeed search buffer (`*elfeed-search*`), you can score all the search results with "= v" (`elfeed-score/score-search`). When the command completes, the view will be re-sorted by score. Your score file will also have been updated on disk (to record the last time that each rule matched).  If you want to see the scoring actions as they're happening, set `elfeed-score/debug` to `t`.

# Status and Roadmap

I'm using `elfeed-score` day in & day out for my RSS reading, but this is a preliminary release (the version number, 0.2.0, was chosen to suggest this).

Things I want to do next:

-   support adding tags based on score (e.g. "if the score is greater than <span class="underline">n</span>, add tag 'foo'")
-   add tag-specific rules (e.g. "only run this scoring rule if the entry is already tagged 'bar'")
-   add whole-word matching
-   add some kind of feature to age out rules that haven't matched in a long time

Bugs, comments, feature requests &c welcome at [sp1ff@pobox.com](sp1ff@pobox.com).