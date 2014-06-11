# Notes on Setting up Cygwin

* Use the cygwin installer to do a minimal install.

* You can get rid of the /cygdrive prefix by issuing
```
mount -c /
mount -m > /etc/fstab
```
which should change /etc/fstab to something like
```
    none / cygdrive binary,posix=0,user 0 0
```
CARE: You will probably need to edit /etc/passwd as well.

* After installation, your home dir will probably be in /home/name, rather
than C:\Users\Phil. The easiest way to change this is to just edit /etc/passwd
in vim, for example to set it to /cygdrive/c/Users/Phil, or /c/Users/Phil
if you have adjusted the cygdrive prefix.

* Use the "apt-cyg" script to install packages; a version is in ~/OtherApps
so it should be on your path (adding that folder to the path is part of the
general setup instructions).

* Setup a shortcut to run Cygwin in mintty (see examples in colors/README.md).

* Get the latest Git (about 30 minutes)
  http://www.gizmoplex.com/wordpress/upgrade-git-to-latest-stable-release-in-cygwin/


