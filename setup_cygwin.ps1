param([Int32]$arch=64)

# setup_cygwin.ps1.
# Installs Cygwin into C:\cywgin. Normally installs the 64-bit version,
# but you can specify another one by passing 32 as a parameter.

Write-Host ("{0} Cygwin setup starting" -f (Get-Date -format s))

$cygDir = "C:\cygwin"

# Create cygwin directory.
if(!(Test-Path -Path "$cygDir" -PathType Container)) {
    $null = New-Item -Type Directory -Path "$cygDir" -Force
    Write-Host "$cygDir created"
}

# Download Cygwin setup.
#$osArch = (gwmi win32_operatingsystem | select osarchitecture).osarchitecture

if($arch -eq 32) {
    $downloadFile = "http://cygwin.com/setup-x86.exe";
    $targetFile = "$cygDir\setup-x86.exe"
} else {
    $downloadFile = "http://cygwin.com/setup-x86_64.exe";
    $targetFile = "$cygDir\setup-x86_64.exe"
}

$client = new-object System.Net.WebClient
$client.DownloadFile($downloadFile, $targetFile);
Write-Host "$downloadFile downloaded to $targetFile"

# Get list of packages to install.
$packagesStr = ((new-object net.webclient).DownloadString('https://raw.githubusercontent.com/PhilipDaniels/dotfiles/master/cygwin_packages.txt'))
$packages = ($packagesStr -split '[\r\n]') |? {$_}
$packageList = [String]::Join(",", $packages);

Write-Host "Installing ===================="
Write-Host $packageList
Write-Host "==============================="


# The following list was created using the cygwin_mirror_test.pl script.
#These are your 20 best choices
#7.24918603897095 sec http://cygwin.cathedral-networks.org/
#7.34770083427429 sec ftp://artfiles.org/cygwin.org/pub/cygwin/
#7.69138693809509 sec http://ftp.fsn.hu/pub/cygwin/
#7.70231890678406 sec http://artfiles.org/cygwin.org/pub/cygwin/
#10.3307569026947 sec http://tweedo.com/mirror/cygwin/
#11.5071229934692 sec ftp://ftp.fsn.hu/pub/cygwin/
#13.2624759674072 sec ftp://ftp.mirrorservice.org/sites/sourceware.org/pub/cygwin/
#13.3484349250793 sec http://mirrors-uk.go-parts.com/cygwin/
#16.3633959293365 sec http://mirrors.chauf.net/cygwin/
#16.9122979640961 sec http://cygwin.localhost.equipment/
#17.7348029613495 sec ftp://mirrors-uk.go-parts.com/cygwin/
#19.0100059509277 sec http://cygwin.mirror.uk.sargasso.net/
#19.6677420139313 sec http://www.mirrorservice.org/sites/sourceware.org/pub/cygwin/
#22.4730589389801 sec ftp://linux.rz.ruhr-uni-bochum.de/cygwin/
#22.6440341472626 sec ftp://ftp-stud.hs-esslingen.de/pub/Mirrors/sources.redhat.com/cygwin/
#22.9422221183777 sec http://ftp-stud.hs-esslingen.de/pub/Mirrors/sources.redhat.com/cygwin/
#23.3495979309082 sec ftp://mirror.switch.ch/mirror/cygwin/
#25.6568729877472 sec ftp://ftp.fit.vutbr.cz/pub/systems/cygwin/
#27.3455760478973 sec ftp://mirror.easyname.at/cygwin/
#28.6803359985352 sec http://bo.mirror.garr.it/mirrors/sourceware.org/cygwin/
#$mirror = 'http://mirror.steadfast.net/cygwin/';   known to work but very slow.
$mirror = 'http://cygwin.cathedral-networks.org/'
Write-Host ("{0} Main Cygwin setup starting" -f (Get-Date -format s))
Start-Process -wait -FilePath $targetFile -ArgumentList ("-q -l $cygDir\packages -s " + $mirror + " -R " + $cygDir + " -P " + $packageList);
Write-Host ("{0} Main Cygwin should now be installed." -f (Get-Date -format s))


$mirror = 'ftp://ftp.cygwinports.org/pub/cygwinports/';
Start-Process -wait -FilePath $targetFile -ArgumentList ("-q -l $cygDir\packages -K http://cygwinports.org/ports.gpg -s " + $mirror + " -R " + $cygDir + " -P " + $packageList);
Write-Host ("{0} Cygports should now be installed." -f (Get-Date -format s))


# Download apt-cyg and put it in the bin folder.
# Download into our home dir to avoid permissions problems.
#Write-Host "Downloading apt-cyg"
#C:\cygwin\bin\bash --noprofile -c "/bin/lynx -source https://raw.githubusercontent.com/transcode-open/apt-cyg/master/apt-cyg > /bin/apt-cyg;/usr/bin/chmod ugo+rx /bin/apt-cyg"
#Write-Host "Downloading apt-cyg completed"

#C:\cygwin\bin\bash --noprofile -c "cd /tmp; /bin/install apt-cyg /bin"
#Write-Host "apt-cyg installed"
#Remove-Item "apt-cyg"


# Edit fstab to remove the cygdrive prefix, so we can type
# /c/Users rather than /cygdrive/c/Users.
C:\cygwin\bin\bash --noprofile -c "/bin/sed -i.bak 's/\/cygdrive/\//g' /etc/fstab"
Write-Host "Removed cygdrive prefix from /etc/fstab"

# By default, Cygwin users /home/uid as your home directory. It's not nice. Change it
# to be the same as the Windows profile.
C:\cygwin\bin\bash --noprofile -c "/bin/sed -i.bak 's/# db_home.*$/db_home: windows/g' /etc/nsswitch.conf"
Write-Host "Patched /etc/nsswitch.conf to set your Cygwin home directory to be the same as your Windows directory"

Write-Host ("{0} Cygwin setup complete." -f (Get-Date -format s))
