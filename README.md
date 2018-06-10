# dotfiles

dotfiles: bash, vim, terminal and misc config for Linux and WSL.

### Linux Installation instructions

This repo is expected to live at ~/repos/dotfiles. It is best to clone it using SSH
as it makes pushing changes easier. First create my .ssh folder and install my keys,
note that the permissions are important: https://superuser.com/questions/215504/permissions-on-private-key-in-ssh-folder

```
mkdir -m700 ~/.ssh
# Copy in my id_phil files...
chmod 600 ~/.ssh/id_phil
chmod 644 ~/.ssh/id_phil.pub
```

Possibly you will need to install keychain and eval it manually the first time:

```
sudo apt install keychain
eval `keychain --quiet --eval id_phil`
```


```
mkdir ~/repos
cd ~/repos
```

```
git clone git@github.com:PhilipDaniels/dotfiles.git       # ssh, or
git clone https://github.com/PhilipDaniels/dotfiles.git   # https

cd dotfiles
./00install_dotfiles.sh
```

For more on SSH: http://philipdaniels.com/blog/2016/12/setup-of-ssh/

### WSL Setup

You need an X Server. VcXsrv is installed by the chocolatey script and seems to work
reasonably well. However, the default "Run on startup" shortcut it creates asks you
some bothersome questions. To fix this, go the startup folder (typically at
`C:\Users\Phil\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup`) and delete
the existing shortcut, and create a new one with a target of

```
  "C:\Program Files\VcXsrv\vcxsrv.exe" -multiwindow  
```

To start-over with WSL, you can do

```
lxrun /uninstall /full
```

This will remove any file system, but leave the new store app installed. A new filesystem
will be built for you the next time you run an Ubuntu prompt.

```
ubuntu clean 
```

Source: https://docs.microsoft.com/en-us/windows/wsl/faq
https://github.com/Microsoft/WSL/issues/2590
https://www.howtogeek.com/261188/how-to-uninstall-or-reinstall-windows-10s-ubuntu-bash-shell


### Chocolatey setup

To install/upgrade chocolatey (and a large list of apps) and setup some environment variables run
this from an elevated PowerShell prompt:

```
./20install_windows_programs.ps1
```

In the future, to upgrade all choco programs do:

```
choco upgrade all -y
```

Chocolatey installs most apps into Program Files normally, but a large proportion are put in
`C:\ProgramData\chocolatey\bin`. This folder should get added to your path automatically.

Your profile is at: %UserProfile%\AppData\Roaming\Microsoft\Windows. Open your Windows Start
Menu folder and create shortcuts to the apps that you will be using frequently. This makes them
available by searching by pressing the "Win" key.

### Fonts

There is a Linux script to install fonts, but I am not sure how good it is. There is
no Windows equivalent at the moment.

Some sources of fonts: 

Download fonts from https://github.com/chrissimpkins/codeface and install them all (search for ttf in the unzipped folder).

### Commands for debugging TCP in Windows

```
nbtstat -n
route print
route delete 0.0.0.0
ipconfig /release
ipconfig /renew
ipconfig /flushdns
```

To reset TCP/IP http://support.microsoft.com/kb/299357 http://www.timdavis.com.au/general/windows-7-default-gateway-0-0-0-0-problem/

```
 netsh int ip reset c:\resetlog.txt
 netsh winsock reset  c:\winsock.txt
``` 

Then reboot.

Check if these services are started and set to automatic.

```
 DHCP Client
 DNS Client
 Remote Procedure Call (RPC)
 TCP/IP Netbios helper
```

### Misc Windows notes

* Do not create Windows VMs using Windows Enterprise because they do not get upgraded to
  Windows 10. Windows anything-but-Enterprise will get upgraded automatically.
* To enable Remote Desktop to virtual machines you need to go into Control Panel (in the
  VM itself) and enable Remote Desktop. Remote Assistance can be disabled.
* When using RDP to connect to a VM, to specify no domain, as for a Linux box, enter the
  user name as "\phil" 
* Setup a fixed IP for a VM by reserving its MAC address on the router and adding it to
  `C:\Windows\System32\drivers\etc\hosts`. If you don't do this it may not be accessible
  by RDP.

### TODO

A decent prompt once more! Must be fast though.
