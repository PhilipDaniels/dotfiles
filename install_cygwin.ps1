# setup.ps1
#   This is intended to...
#   - Setup environment variables
#   - Setup cygwin with all packages
#   - Setup ubuntu font
#   - Anything else that needs setting up on Windows profile

$cygDir = "C:\cygwin"

# Create the environment variables that various programs need.
[Environment]::SetEnvironmentVariable("CYGROOT", $cygDir, "User")
[Environment]::SetEnvironmentVariable("FARROOT", "C:\Public\Apps\Far30b4242.x64.20150117", "User")
[Environment]::SetEnvironmentVariable("PORTABLEAPPSROOT", "C:\PortableApps", "User")
[Environment]::SetEnvironmentVariable("PUBLICROOT", "C:\Public", "User")

$cygDir = "C:\cygwin2"

# Create cygwin directory.
if(!(Test-Path -Path "$cygDir" -PathType Container)) {
    $null = New-Item -Type Directory -Path "$cygDir" -Force
}

# Download Cygwin setup.
$osArch = (gwmi win32_operatingsystem | select osarchitecture).osarchitecture

if($osArch -eq "32-bit") {
    $downloadFile = "http://cygwin.com/setup-x86.exe";
    $targetFile = "$cygDir\setup-x86.exe"
} else {
    $downloadFile = "http://cygwin.com/setup-x86_64.exe";
    $targetFile = "$cygDir\setup-x86_64.exe"
}

$client = new-object System.Net.WebClient
$client.DownloadFile($downloadFile, $targetFile);

# Get list of packages to install.
$mirror = 'http://mirrors.kernel.org/cygwin/';

$packagesStr = ((new-object net.webclient).DownloadString('https://raw.githubusercontent.com/philipdaniels/dotfiles/master/cygwin_packages.txt'))
$packages = ($packagesStr -split '[\r\n]') |? {$_}
$packageList = [String]::Join(",", $packages);

"Installing ===================="
$packageList
"==============================="

return







$TempCygDir = "$env:temp\cygInstall";



Start-Process -wait -FilePath "$TempCygDir\setup.exe" -ArgumentList ("-q -n -l $TempCygDir -s " + $mirror + " -R " + $cygInstallFolder + " -P " + $packageList);

"Cygwin should now be installed."

$gitHubRoot = "https://raw.githubusercontent.com/aikeru/sauce/master/";
$installFontUrl = $gitHubRoot + "scripts/Add-Font.ps1"
$fontUrl = $gitHubRoot + "assets/Ubuntu Mono for Powerline_0.ttf"

$client.DownloadFile($installFontUrl, "$TempCygDir\Add-Font.ps1");
$client.DownloadFile($fontUrl, "$TempCygDir\Ubuntu Mono for Powerline_0.ttf");

iex($TempCygDir + '\Add-Font.ps1 -Path "' + $TempCygDir + '\Ubuntu Mono for Powerline_0.ttf"');
  
"Installing Chocolatey..."

iex ((new-object net.webclient).DownloadString('https://chocolatey.org/install.ps1'))

"Chocolatey should be installed ..."

choco install nodejs

"Node.js should be installed now..."

