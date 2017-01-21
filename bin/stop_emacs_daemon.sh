#!/bin/bash

if type emacs-w32 > /dev/null; then
    emacsclient-w32 -e '(kill-emacs)'
else
    emacsclient -e '(kill-emacs)'
fi

