# Disks
512GB Samsung (boot drive)
3TB HDD (/mnt/sharingdrive)
14TB HDD (/mnt/datadrive)
1TB SSD (/mnt/torrentdrive)

# Other Hardware
Motherboard is Asus P9Z77-V
Press DEL to get into the BIOS.
There might be a boot menu available by pressing ESC or F8.

It WILL boot from USB, but only from a USB2.0 port (on top).
Likewise, in the BIOS, the radio mouse will only work from USB2.0.
You may need to reselect the primary boot device to be USB each
time you boot (e.g. if you remove and reinsert the USB device it
will forget the boot order).


# Preparation
Run the backup script in my home folder to backup everything to the data drive.
Then backup everything to external HDD.

WiFi: Ensure you have the password handy, this is the only networking I have

Virtual Box
  MUST GO TO THEIR WEBSITE TO GET THE DEB.
  Remove all mapped/shared drives before backing up the VM.
  Once up and running again, copy the backup to the new location.
  The location should be $HOME\VirtualBox VMs
    See https://forums.virtualbox.org/viewtopic.php?f=35&t=55003

Anki
  Do File -> Export -> Collection.colpkg for each profile
  See https://docs.ankiweb.net/exporting.html


# Installation
I used KDE Neon because Kubuntu crashed to a black screen.
During install it was not connected to the WiFi, so some
things were not installed. In particular, I did not get
NVidia drivers so the display was very slow.

NVidia from https://tutorialforlinux.com/2020/10/28/step-by-step-driver-nvidia-kde-neon-linux-installation/2/
At the time of writing the latest was 4.65.
  apt search nvidia-driver*
  sudo aptitude install nvidia-driver-465 nvidia-settings

This made all the fonts bigger, so I had to go into Settings -> Appearance ->
Fonts and force 96dpi.


# Post Installation
## Networking
My Windows box is at 10.0.0.2.
Router is at fritz.box
Printer - is on WiFi only

## Hard Drives
See https://wiki.archlinux.org/title/hdparm#Power_off_a_hard_disk_drive
and https://wiki.archlinux.org/title/Udev#Identifying_a_disk_by_its_serial

If hard drives are noisy, you can make them spin down after N minutes of activity
by creating a file called '/etc/udev/rules.d/69-hdparm.rules' containing a line
of the form (for one specific drive):

    ACTION=="add", KERNEL=="sd[a-z]", ENV{ID_SERIAL_SHORT}=="9MGWMJVJ", RUN+="/usr/sbin/hdparm -S 120 /dev/%k"

or for all spinning drives:

    ACTION=="add|change", KERNEL=="sd[a-z]", ATTRS{queue/rotational}=="1", RUN+="/usr/bin/hdparm -S 60 /dev/%k"

## VirtualBox
Setup USB passthrough rules - connect the USB device first, it should then
appear in the dialog box for you to add a rule just by pressing a button.

I should setup my entire Home directory as a shared folder in Virtual Box.
https://docs.oracle.com/en/virtualization/virtualbox/6.0/user/sharedfolders.html

Garmin ETrex Vista HCx: had to follow this guide
  https://ubuntuforums.org/showthread.php?t=2391983
and could then use GPSBabel to communicate with it from Linux. However,
it still does not show up in VirtualBox. I issued
  sudo adduser $USER vboxusers
and rebooted, and then I could see all the USB devices in the Devices menu.
I got this from this page: https://superuser.com/questions/956622/no-usb-devices-available-in-virtualbox
but the above step was the only one I had to follow. In particular, I did
not have to install the extension pack.

# Sharing
I share my entire home drive as an SMB share and connect to it from my HTPC.
May need to install Samba??? something to be able to do this from Dolphin.

# Apps
I have a dir at ~/.local/bin which can be used to hold binaries.

KeepassXC: I keep my passwords at Documents\Personal Passwords.kdbx

Calibre: My collection is at /home/phil/Documents/Calibre Library

Micro Editor: https://github.com/zyedidia/micro#installation
    Set system editor to micro. This will change it in Midnight Commander
    , among others. Note that gitui picks up the editor from ~/.gitconfig.

Running 'cargo install' will update if a newer version is available
    cargo install ripgrep
    cargo install fd-find
    cargo install gitui
    cargo install tokei
    cargo install bottom (crashes?)
    wezterm: from source, clone the repo https://github.com/wez/wezterm

cargo-add
cargo-watch
cargo-upgrade
cargo-outdated

