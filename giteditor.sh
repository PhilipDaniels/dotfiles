#!/bin/sh --noprofile

# This script is run when doing a Git Commit to pick the editor.
# http://emacs.stackexchange.com/questions/2417/open-edit-server-files-from-emacsclient-in-a-specific-frame?rq=1

if [ "$WINDIR" == "" ]; then
    # Running on Linux.
    if [ "$INSIDE_EMACS" == "" ]; then
        emacsclient -c $1;
    else
        emacsclient $1;
    fi
else
    if [ "$INSIDE_EMACS" == "" ]; then
        emacsclient-w32 $1;
    else
        emacsclient-w32 $1;
    fi
fi



