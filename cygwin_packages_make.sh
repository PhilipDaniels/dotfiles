#!/bin/bash

# Generates the list of currently installed Cygwin packages, which is
# used by the setup_cygwin.ps1 script.

cygcheck -c -d | sed -e "1,2d" -e 's/ .*$//' | sort -u > cygwin_packages.txt
