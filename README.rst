==============
|build-status|
==============

A Gnus backend for Reddit.

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
``make install`` from github source.

Usage
=====
In your ``.gnus`` or ``.emacs``,

::

   (add-to-list 'gnus-secondary-select-methods
                '(nnreddit ""))

A typical session might look like::

   M-x gnus
   R g emacs
   q
   u

Now we're subscribed to ``r/emacs``.  Read the first unread article with ``RET``.  Rapidly catch yourself up via ``N`` and ``P``.

Create a post via ``a``.

Reply to articles with ``f`` or ``r``.

Vote articles by first entering the Article buffer, then ``R -`` (down), ``R =`` (up), or ``R 0`` (retract).

.. _Cask: https://cask.readthedocs.io/en/latest/guide/installation.html
.. _MELPA: http://melpa.org/#/
