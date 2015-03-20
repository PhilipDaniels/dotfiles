# For Visual Studio, these are the folders where snippets are stored.
# Default snippets : C:\Program Files (x86)\Microsoft Visual Studio {0}\LANGUAGE\Snippets   (varies by VS version, SQL_SSDT affects SQL files)
# User snippets    : %MYDOCUMENTS%\Visual Studio {0}\Code Snippets                          (varies by VS version)


$myDocs = [Environment]::GetFolderPath("mydocuments")  # Differs on Win7/8.

# Clean Visual Studio snippets.
foreach ($vsVer in 2012,2013,2015)
{
    $snippetDir = "{0}\Visual Studio {1}\Code Snippets" -f $myDocs, $vsVer

    if (Test-Path $snippetDir)
    {
        foreach ($file in Get-ChildItem $snippetDir -Recurse -Filter "*.snippet")
        {
			Remove-Item $file.FullName
			Write-Host ("Deleted snippet {0}" -f $file.FullName)
        }
    }
}
