#!/bin/bash

if type emacs-w32 > /dev/null 2&>1; then
    emacs-w32 --daemon
else
    emacs --daemon
fi
