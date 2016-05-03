#!/usr/bin/bash
# Make tags for emacs (excluding generated codes not to find swapper macros)
find test* -name "*.[ch]pp" | xargs ctags -e
