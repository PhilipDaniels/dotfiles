# dotfiles

dotfiles: bash, vim, terminal and misc config for Linux and WSL.

Also see [Linux Reinstallation Notes](linux_reinstallation.md)
and [Fonts](fonts.md).


### Linux Installation instructions

Do the below - then see the comment in mybashrc.sh.

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
```

For more on SSH: http://philipdaniels.com/blog/2016/12/setup-of-ssh/


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
