See http://askubuntu.com/questions/63588/how-do-i-get-fan-control-working

i8kutils was got with:

    apt-get source i8kutils

smm.c is the source code which is compiled with the command line (assuming a
64 bit system):

    sudo apt-get install g++-multilib         # for sys\cdefs.h
    gcc -g -O2 -Wall -I. -o smm -m32 smm.c

To disable fan control use

    sudo ./smm 30a3

To reenable fan control use

    sudo ./smm 31a3

Or use the fanon and fanoff scripts.

Contrary to what the article says, I have found that it does not always persist:

  Shutdown + restart : persists
  Hibernate          :
  Suspend            :
