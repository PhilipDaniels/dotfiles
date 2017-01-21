#!/bin/bash

if type emacs-w32 > /dev/null; then
    emacs-w32 --daemon
else
    emacs --daemon
fi
