#$scriptPath = split-path -parent $MyInvocation.MyCommand.Definition
<#
.SYNOPSIS
Adds one or more folders to the Path environment variable.

.PARAMETER Folders
Specifies the folders to add to the Path environment variable..

.PARAMETER EnvironmentVariableTarget
Specifies the "scope" to use for the Path environment variable ("Process",
"Machine", or "User"). Defaults to "User" if the parameter is not specified.

.EXAMPLE
.\Add-PathFolders.ps1 C:\NotBackedUp\Public\Toolbox
#>
function Add-PathFolders
{
    param(
    [parameter(Mandatory = $true, ValueFromPipeline = $true)]
    [string[]] $Folders,
    [string] $EnvironmentVariableTarget = "User")

    begin
    {
        Set-StrictMode -Version Latest
        $ErrorActionPreference = "Stop"

        Write-Verbose "Path environment variable target: $EnvironmentVariableTarget"

        [bool] $isInputFromPipeline =
            ($PSBoundParameters.ContainsKey("Folders") -eq $false)

			[int] $foldersAdded = 0

        [string[]] $pathFolders = [Environment]::GetEnvironmentVariable(
            "Path",
            $EnvironmentVariableTarget) -Split ";"

        [Collections.ArrayList] $folderList = New-Object Collections.ArrayList

        $pathFolders | foreach {
            $folderList.Add($_) | Out-Null
        }
    }

    process
    {
        If ($isInputFromPipeline -eq $true)
        {
            $items = $_
        }
        Else
        {
            $items = $Folders
        }

        $items | foreach {
            [string] $folder = $_

            [bool] $isFolderInList = $false

            $folderList | foreach {
                If ([string]::Compare($_, $folder, $true) -eq 0)
                {
                    Write-Verbose ("The folder ($folder) is already included" `
                        + " in the Path environment variable.")

                    $isFolderInList = $true
                    return
                }
            }

            If ($isFolderInList -eq $false)
            {
                Write-Verbose ("Adding folder ($folder) to Path environment" `
                    + " variable...")

                $folderList.Add($folder) | Out-Null
                $foldersAdded++
            }
        }
    }

    end
    {
        If ($foldersAdded -eq 0)
        {
            Write-Verbose ("No changes to the Path environment variable are" `
                + " necessary.")
            return
        }

        [string] $delimitedFolders = $folderList -Join ";"

        [Environment]::SetEnvironmentVariable(
            "Path",
            $delimitedFolders,
            $EnvironmentVariableTarget)

        Write-Verbose ("Successfully added $foldersAdded folder(s) to Path" `
            + " environment variable.")
    }
}


Write-Host ("{0} Starting setup_choco.ps1" -f (Get-Date -format s))
Write-Host ("{0} Setting up the CYGROOT environment variable (for ConEmu paths, etc.)" -f (Get-Date -format s))
[Environment]::SetEnvironmentVariable("CYGROOT", "C:\cygwin", "User")

# Install chocolatey itself.
Write-Host ("{0} Downloading Chocolatey..." -f (Get-Date -format s))
iex ((new-object net.webclient).DownloadString('https://chocolatey.org/install.ps1'))
Write-Host ("{0} Chocolatey now installed." -f (Get-Date -format s))

Write-Host ("{0} Adding C:\ProgramData\chocolatey\bin to the PATH." -f (Get-Date -format s))
Add-PathFolders "C:\ProgramData\chocolatey\bin"

Write-Host ("{0} Installing chocolatey packages" -f (Get-Date -format s))


choco install 7zip -y
choco install 7zip.commandline -y
choco install audacity -y
choco install autoruns -y
#choco install blender -y
choco install conemu -y
choco install cpu-z -y
choco install dependencywalker -y
choco install defaultprogramseditor -y
choco install dotpeek -y
#choco install eclipse -y
#choco install emacs -y
#choco install evince -y
choco install far -y
choco install fiddler4 -y
choco install filetypesman -y
choco install filezilla -y
choco install firefox -y
choco install freefilesync -y
choco install frhed -y
#choco install geany -y
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
choco install patheditor -y
choco install p4merge -y
choco install paint.net -y
choco install pdfcreator -y
choco install pencil -y
choco install pester -y
choco install pidgin -y
choco install procexp -y
choco install putty -y
#choco install ransack -y
choco install regshot -y
choco install shmnview -y
choco install skype -y
choco install sumatrapdf -y
choco install swissfileknife -y
choco install sysinternals -y
choco install teamviewer -y
choco install unetbootin -y
#choco install qbittorrent -y
#choco install vim -y
choco install virtualbox -y
choco install virtualclonedrive -y
#choco install virtuawin -y
choco install vlc -y
choco install windirstat -y
choco install winmerge -y

# Things not got that I have in PortableApps.com: CamStudio, dotNetInspector, Games

Write-Host ("{0} Completed setup_choco.ps1" -f (Get-Date -format s))
