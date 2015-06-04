Write-Host ("{0} Starting setup_choco.ps1" -f (Get-Date -format s))

# Install chocolatey itself
iex ((new-object net.webclient).DownloadString('https://chocolatey.org/install.ps1'))
Write-Host ("{0} Chocolatey installed, moving onto the apps..." -f (Get-Date -format s))

choco install 7zip -y
choco install 7zip.commandline -y
choco install audacity -y
choco install autoruns -y
#choco install blender -y
choco install cpu-z -y
choco install dependencywalker -y
choco install dotpeek -y
choco install eclipse -y
choco install evince -y
choco install fiddler4 -y
choco install filezilla -y
choco install firefox -y
choco install freefilesync -y
choco install frhed -y
choco install geany -y
choco install gimp -y
choco install googlechrome -y
choco install inkscape -y
choco install irfanview -y
choco install irfanviewplugins -y
choco install kdiff3 -y
choco install keepass -y
choco install libreoffice -y
choco install linqpad4 -y
choco install nugetpackageexplorer -y
choco install notepadplusplus -y
choco install opera -y
choco install p4merge -y
choco install paint.net -y
choco install pdfcreator -y
choco install pencil -y
choco install pester -y
choco install pidgin -y
choco install procexp -y
choco install putty -y
choco install ransack -y
choco install regshot -y
choco install skype -y
choco install sumatrapdf -y
choco install sysinternals -y
choco install teamviewer -y
choco install unetbootin -y
choco install qbittorrent -y
choco install vim -y
choco install virtualbox -y
choco install virtualclonedrive -y
choco install virtuawin -y
choco install vlc -y
choco install windirstat -y
choco install winmerge -y
# Things not got that I have in PortableApps.com: CamStudio, dotNetInspector, Games

Write-Host ("{0} Completed setup_choco.ps1" -f (Get-Date -format s))
