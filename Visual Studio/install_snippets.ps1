# For Visual Studio, these are the folders where snippets are stored.
# Default snippets : C:\Program Files (x86)\Microsoft Visual Studio {0}\LANGUAGE\Snippets   (varies by VS version, SQL_SSDT affects SQL files)
# User snippets    : %MYDOCUMENTS%\Visual Studio {0}\Code Snippets                          (varies by VS version)

# For SSMS you first have to register the folder where you are storing the snippets using the "Add..." button
# to be found under the Tools -> Code Snippets Manager menu item.
# After that, it will pick up any changes automatically via a File-Watch.
# Default snippets : C:\Program Files (x86)\Microsoft SQL Server\110\Tools\Binn\ManagementStudio\SQL\Snippets\1033     (varies by SSMS version)


# Special case for SQL snippets
# =============================
# For Visual Studio .sql files, the language in the snippet file must be "SQL_SSDT".
# For SSMS, the language must be "SQL" and the snippet must be imported/registered manually using the GUI,
# but we can leave it in the original folder.

# Therefore our design is:
# 1. The snippets on disk in this repo are in the SQL_SSDT folder.
# 2. The language in them is "SQL".
# 3. We manually import that folder into SSMS.
# 4. The installation for VS is done in the loop below, which detects the snippets in the
#    SQL_SSDT folder and flips the language to SQL_SSDT before copying them to the folder.

# To use these snippets in Visual Studio and SSMS, the shortcut is Ctrl-K, X.

$scriptDir = Split-Path $MyInvocation.MyCommand.Path -Parent
$myDocs = [Environment]::GetFolderPath("mydocuments")  # Differs on Win7/8.

# Install Visual Studio snippets.
$sourceDir = "$scriptDir\Code Snippets"
foreach ($vsVer in 2012,2013,2015)
{
    $snippetDir = "{0}\Visual Studio {1}\Code Snippets" -f $myDocs, $vsVer
    #$snippetDir = "C:\temp\tst"

    if (Test-Path $snippetDir)
    {
        foreach ($srcFile in Get-ChildItem $sourceDir -Recurse -Filter "*.snippet")
        {
            $dstFile = Join-Path $snippetDir $srcFile.FullName.Substring($sourceDir.Length)
            $dstDir = Split-Path $dstFile -Parent
            New-Item $dstDir -Type Directory -Force > $null

            if ($dstDir.Contains("SQL_SSDT"))
            {
                Get-Content $srcFile.FullName | ForEach-Object { $_ -replace 'Code Language="SQL"', 'Code Language="SQL_SSDT"' } | Set-Content -Path $dstFile -Force
                Write-Host "Wrote SQL snippet with changed language to to $dstFile"
            }
            else
            {
                Copy-Item -Force -Path $srcFile.FullName -Destination $dstFile
                Write-Host "Copied snippet to $dstFile"
            }
        }
    }
    else
    {
        Write-Host "$snippetDir for Visual Studio DOES NOT EXIST - snippets not copied."
    }
}
