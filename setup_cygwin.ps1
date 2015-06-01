param([Int32]$arch=64)

# setup_cygwin.ps1.
# Installs Cygwin into C:\cywgin. Normally installs the 64-bit version,
# but you can specify another one by passing 32 as a parameter.

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

$mirror = 'http://mirror.steadfast.net/cygwin/';
Start-Process -wait -FilePath $targetFile -ArgumentList ("-q -l $cygDir\packages -s " + $mirror + " -R " + $cygDir + " -P " + $packageList);
Write-Host "Main Cygwin should now be installed."

$mirror = 'ftp://ftp.cygwinports.org/pub/cygwinports/';
Start-Process -wait -FilePath $targetFile -ArgumentList ("-q -l $cygDir\packages -K http://cygwinports.org/ports.gpg -s " + $mirror + " -R " + $cygDir + " -P " + $packageList);
Write-Host "Cygports should now be installed."


# Download apt-cyg and put it in the bin folder.
# Download into our home dir to avoid permissions problems.
Write-Host "Downloading apt-cyg"
C:\cygwin\bin\bash --noprofile -c "/bin/lynx -source https://raw.githubusercontent.com/transcode-open/apt-cyg/master/apt-cyg > /bin/apt-cyg;/usr/bin/chmod ugo+rx /bin/apt-cyg"
Write-Host "Downloading apt-cyg completed"

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


# TODO: Mymintty
