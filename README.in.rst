==============
|build-status|
==============

.. COMMENTARY (see Makefile)

.. |build-status|
   image:: https://travis-ci.org/dickmao/nnreddit.svg?branch=master
   :target: https://travis-ci.org/dickmao/nnreddit
   :alt: Build Status
.. |melpa-dev|
   image:: http://melpa.milkbox.net/packages/ein-badge.svg
   :target: http://melpa.milkbox.net/#/ein
   :alt: MELPA development version
.. |melpa-stable|
   image:: http://melpa-stable.milkbox.net/packages/ein-badge.svg
   :target: http://melpa-stable.milkbox.net/#/ein
   :alt: MELPA stable version

Install
=======
As described in `Getting started`_, ensure melpa's whereabouts in ``init.el`` or ``.emacs``::

   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

Then

::

   M-x package-refresh-contents RET
   M-x package-install RET nnreddit RET

Alternatively, directly clone this repo and ``make install``.

Usage
=====
In your ``.gnus`` or ``.emacs``,

::

   (add-to-list 'gnus-secondary-select-methods
                '(nnreddit ""))

A typical session might look like::

   M-x gnus
   R g emacsy
   q
   u

Keybinding ``u`` (gnus-group-unsubscribe-current-group) actually *toggles* subscription to ``r/emacsy``.  I don't make the rules.

Reenter the ``emacsy`` newsgroup with ``RET``.  Rapidly catch yourself up via ``N`` and ``P``.  Instantly catch-up with ``c``.

Create a post via ``a``.

Reply to articles with ``f`` or ``r``.  Include original with ``F``.

Vote articles by first entering the Article buffer, then ``R -`` (down), ``R =`` (up), or ``R 0`` (retract).

From the ``*Group*`` buffer, press ``g`` to refresh all subreddits.  ``M-g`` on a particular subreddit to refresh individually.

From the summary buffer, ``/o`` redisplays articles already read.  ``x`` undisplays them.

``S s`` edits articles.

``S c`` cancels articles.

Gnus beginners may find the interface bewildering.  In particular, subreddits with no unread articles do not display.  Use ``L`` to bring them out of hiding.

.. _Cask: https://cask.readthedocs.io/en/latest/guide/installation.html
.. _Getting started: http://melpa.org/#/getting-started
