#!/bin/sh --noprofile

# This script is run when doing a Git Commit to pick the editor.
# http://emacs.stackexchange.com/questions/2417/open-edit-server-files-from-emacsclient-in-a-specific-frame?rq=1

if [ "$WINDIR" == "" ]; then
    # Running on Linux.
    if [ "$INSIDE_EMACS" == "" ]; then
        # Running inside an external terminal emulator.
        emacsclient -q --tty $1;
    else
        # Running inside Emacs - probably ansi-term or Magit.
        emacsclient $1;
    fi
else
    # Windows, probably Cygwin.
    emacsclient-w32 -q $1;

    # if [ "$INSIDE_EMACS" == "" ]; then
    #     # Running inside an external terminal emulator.
    #     # emacsclient-w32 --tty $1;   # This causes Emacs to hang.
    #     emacsclient  $1;
    # else
    #     # Running inside Emacs - probably ansi-term or Magit.
    #     emacsclient-w32 $1;
    # fi
fi



