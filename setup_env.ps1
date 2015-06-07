# Sets up the main environment.

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

# setup.ps1 - Main installation script. Now attempt to automate
# all steps documented in INSTALL.md.

Write-Host ("{0} Starting setup_env.ps1" -f (Get-Date -format s))

# Create the environment variables that various programs need.
[Environment]::SetEnvironmentVariable("CYGROOT", $cygDir, "User")
[Environment]::SetEnvironmentVariable("FARROOT", "C:\Public\Apps\Far30b4242.x64.20150117", "User")
[Environment]::SetEnvironmentVariable("PORTABLEAPPSROOT", "C:\PortableApps", "User")
[Environment]::SetEnvironmentVariable("PUBLICROOT", "C:\Public", "User")
Write-Host "Environment variables created"


# Install chocolatey itself.
iex ((new-object net.webclient).DownloadString('https://chocolatey.org/install.ps1'))
Write-Host ("{0} Chocolatey (but no apps) installed." -f (Get-Date -format s))

Add-PathFolders "C:\Public\Apps"
Add-PathFolders "C:\ProgramData\chocolatey\bin"

Write-Host ("{0} Completed setup_env.ps1" -f (Get-Date -format s))
