# Instructions

Nada. Just throw the entire .emacs.d directory in your home path and
make sure that you don't have a .emacs file there (since the otherwise
Emacs will read from ~/.emacs instead of ~/.emacs.d/init.el).

The .el files in the top directory is automatically byte-compiled when
loaded by init.el. If you like, you can byte-compile the stuff in
the other/ directory as well (those are entire packages written by others).

# Acknowledgement

These are my emacs config files (sans the personal stuff). I wrote
some of the elisp and tweaked a lots of them, but most of the others
are collected over the period of a few years from various sources. I'm
not so good at recording where I have found them, so my thanks to all of
you who see your own code in my config!