This document should be gone through in order.

Initial Installation
====================
* Setup will be to %UserProfile%, typically C:\Users\Phil.
Do NOT set a HOME variable, Cygwin and MSysGit will use this folder by default
without it. It is known as ~ in this document.

* Restore zips/copy from another machine, you should end up with
	~\Backups                     - backup scripts
    ~\OtherApps                   - portable binaries
    ~\PortableApps                - my PortableApps installation
    ~\Public                      - things I don't mind being copied
    ~\Autorun.inf, ~\Start.exe    - PortableApps files.

	NOTE THAT MY "DOCUMENTS" FOLDER IS PRIVATE AND SHOULD NOT BE COPIED.

* Start menu setup
Your profile is at: %UserProfile%\AppData\Roaming\Microsoft\Windows
Open your Windows Start Menu folder and create shortcuts to the apps that you
will be using frequently. This makes them available by searching by pressing
the "Win" key. Defaults, assuming you used %UserProfile% as mentioned in step
1, are in this folder.

* Path Setup
Use the PathEditor.exe (in the OtherApps folder) to edit your Windows path to
add useful folders to your path. Most things you might want to run from the
command line are available in a Cygwin-bash so typically this is just a small
selection of utilities from the OtherApps folder itself

* Fonts setup
Install fonts from the ~\Public\Fonts folder

* gVim
Check the .reg file and run it to create an "Edit With Vim" Explorer shortcut. 

Cygwin
======
THIS MUST BE INSTALLED ON EACH PC BECAUSE ITS INSTALLER DOES SPECIAL THINGS.

Use 32 bit setup because it still has more packages than 64 bit.

Install base packages: autoconf, automake, bison, clang, cmake, ctags, curl,
cygport, g++, gcc, gdb, git, llvm, make, m4, patch, ping, rsync, screen,
tig, tmux, tree, vim

Edit /etc/fstab so it looks like this, to remove the cygdrive prefix:
  none / cygdrive binary,posix=0,user 0 0

After installation, your home dir will probably be in /home/name, rather
than C:\Users\Phil. The easiest way to change this is to just edit /etc/passwd
in vim, for example to set it to /cygdrive/c/Users/Phil, or /c/Users/Phil
if you have adjusted the cygdrive prefix.

Use the "apt-cyg" script to install packages; a version is in ~/OtherApps
so it should be on your path (adding that folder to the path is part of the
general setup instructions).

SSH Setup
=========
Skip forward and do Appendex 1. Then come back here.

Repos + dotfiles itself
=======================
mkdir ~/repos
git clone git@github.com:PhilipDaniels/dotfiles.git
Run the 00install.sh script to setup .bashrc rc. 

  If you are installing a Linux box then 20main.sh has some frequently
  desired programs. Otherwise, see the install_snippets.ps1 script in the
  Visual Studio folder to install C# and SQL snippets.

  
  
  
  
                        Appendix 1 - SSH Setup
						======================

How SSH Works
=============
SSH stands for "Secure Shell". As its name implies it is a network protocol
for secure communication between two computers, a server - your VM - and a
client, e.g. your work or home PC. The most basic type of communication SSH
supports is shell access - you use a terminal emulator to open a Unix shell
on the server, which then allows you to use the full gamut of Unix commands
to administer the machine. Layered on top of the basic SSH protocol are
higher level protocols such as secure copy (scp) and secure ftp (sftp), which
are useful for copying files into and out of the VM.

SSH allows you to login to a remote machine without a password. There is an
SSH server running on the remote machine. It listens for SSH connection
requests from client machines. When it receives a connection request it
validates the public key from the client against a copy stored (on the server)
in the relevant user's authorized_keys file.

To setup the client you first generate a public/private key pair on that
machine. You copy the public key up to the server and keep the private key on
your client machine. (Some people use multiple private keys, one per user or
one per client, others choose to share them. Whatever you choose to do you
should think of the private key as a "super password", and keep it secret just
as you would do a password.)


Creating a Key
==============
Open a Git-Bash or Cygwin-Bash prompt. Type

  ssh-keygen -t rsa -b 2048

This generates an RSA-2 type key of size 2048 bits. These are the defaults and
are considered secure. You can leave the passphrase blank but if you do this
it means that anyone who gets hold of your private key file will be able to
logon to the VM without a password. On the other hand, if you enter a
passphrase you will be asked to enter the passphrase again each time you use
the key. It effectively functions as a "super long password". You should use a
passphrase which is fairly long but easy to remember and with a mixture of
upper and lower case. There are utilities available that you can run which
means you only have to enter it once per session - see the section below,
"Typing your PassPhrase only once".


Installing the Key on Unix Servers (Virtual Machines etc.)
==========================================================
When you SSH to the server, you are doing it as a particular user, e.g. root.
In that user's home folder there needs to be a file, ~/.ssh/authorized_keys,
to which we will append the public key. You can do this manually, but it is
easier to use the ssh-copy-id program:

  ssh-copy-id -i ~/.ssh/id_phil root@192.168.0.104

    Alternative Manual Technique
    ============================
    If all else fails you can just open the files in a text editor and paste
    the ".pub" key in.

    The authorized_keys file is just a text file with each public key on a
    separate line, so if you don't have access to the ssh-copy-id program you
    can get the same result simply by appending the entire contents of your
    public key file to the authorized_keys file. Here is a complete manual
    script to do this. First, on the VM, do this to create the file if it does
    not already exist:

      mkdir -m700 .ssh
      touch .ssh/authorized_keys
      chmod 600 .ssh/authorized_keys

    Then on the client, do this to copy your public key to the server and append
    it to the authorized_keys file:

      cat ~/.ssh/id_rsa.pub | ssh root@192.168.0.104 "cat  >> ~/.ssh/authorized_keys"

    Pay careful attention to the "punctuation"! The right angle-brackets must
    be double as shown, or you'll wipe out any keys you already have; and the
    quotes must be as shown, or you won't get the desired result. Also be
    careful to cat the public file, not the private one, this is an easy mistake
    to make if you are using tab-completion to enter the filename.

Installing the Key on Github and Bitbucket
==========================================
Login to Bitbucket (you can do this using a Google login). Go to the Management
screen, there you will see a page for "SSH keys". Simply open the id_*.pub file
and paste it in.

Login to Github (you must use a UID and PWD to do this). Go to Account
Settings, there you will see a page for "SSH keys". Simply open the id_*.pub
file and paste it in.

Practical setup of the SSH key
==============================
SSH agents
Every time you use the key you will have to type in your passphrase unless you
use an agent which will only ask you once and then cache it. This is highly
recommended for practical use. My dotfiles bashrc now does this.

Also create the file ~/.ssh/config and add "ForwardAgent yes"

  echo "ForwardAgent yes" > ~/.ssh/config
  echo "ForwardX11Trusted yes" > ~/.ssh/config

This will allow you to SSH into a remote machine to pull from there without
having to place your private key in the remote machine for it to work with your
SSH credentials. The second line enables trusted X11 forwarding by default (it
is the same as always using the -Y option).

Using X11 Forwarding
====================
To forward X:

  ssh [-v] -Y phil@deb1

  * The -Y option supercedes the -X option.
  * The -v is verbose help. To confirm that ssh is forwarding X11, check for a
    line containing "Requesting X11 forwarding" in the output.

On the server:
  cat /etc/ssh/sshd_config | grep X11
There must be a line "X11Forwarding yes"

So, step by step to forward X11 from my deb1 box to Cygwin

  1. [Cygwin] runx               # Start the X server on my Windows box
  2. [Cygwin] DISPLAY=:0.0 ssh -Y phil@deb1
  3. [deb1] xeyes &

Debugging: http://x.cygwin.com/docs/faq/cygwin-x-faq.html#q-ssh-no-x11forwarding
           http://unix.stackexchange.com/questions/12755/how-to-forward-x-over-ssh-from-ubuntu-machine

Using your Key with FileZilla or WinSCP
=======================================
Both FileZilla and WinSCP do not support unix-format SSH keys. It needs to be
converted to a Putty private key and you should use Pageant to cache the key.

(From http://meinit.nl/using-your-openssh-private-key-in-putty
 and  https://wiki.filezilla-project.org/Howto)
Run the PuttyGen program which is in the PortableApps folder. Do
Conversions -> Import key and open "id_phil". Enter a description, otherwise
you won't be able to tell which passphrase to use when Pageant asks you for
it. Press Save Private Key and save it as id_phil.ppk (PuTTY private key).
It doesn't matter about the public half. Then run Pageant and load the key.
To run Pageant at startup, create a shortcut in your Windows Startup folder

  Target = \\Path\To\Pageant.exe id_phil.ppk other.ppk
  Start in = C:\PublicHome\.ssh

Then in FileZilla, just enter
  host = sftp://servername
  username = theuser  (where this is the name of the user on the server
                       that you copied the public part of the key up to)

Summary of Actual Setup
=======================
In Git-Bash, I entered the filename as ~/.ssh/id_phil and the passphrase
was "the historic one". You should name the file "id_something" because various
SSH utilities pattern match against that. There are two files created - id_phil
which is your private key - do not copy this up to any servers! - and
id_phil.pub which is the public key which you copy up to Github or a VM.

