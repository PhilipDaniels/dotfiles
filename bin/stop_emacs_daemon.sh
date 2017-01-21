#!/bin/bash

if type emacsclient-w32 > /dev/null 2>&1; then
    emacsclient-w32 -e '(kill-emacs)'
else
    emacsclient -e '(kill-emacs)'
fi

