# See https://www.gnu.org/software/bash/manual/html_node/Bash-Startup-Files.html
# Unfortunately MSysGit (Git Bash) has the wrong behaviour. It only calls
# ~/.bashrc, so we are forced to put everything in there. 
# DO NOT USE THIS FILE. JUST LEAVE THE LINE BELOW.
# echo "***** Running ~/.bash_profile"

if [ -f .bashrc ]; then . .bashrc; fi

