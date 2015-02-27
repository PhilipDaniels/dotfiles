$scriptDir = Split-Path $MyInvocation.MyCommand.Path -Parent

foreach ($vsVer in 2012,2013,2015)
{
    $sourceDir = "$scriptDir\Code Snippets"
    $snippetDir = "{0}\My Documents\Visual Studio {1}\Code Snippets" -f $env:UserProfile, $vsVer
    #$snippetDir = "C:\temp\tst"

    if (Test-Path $snippetDir)
    {
        foreach ($srcFile in Get-ChildItem $sourceDir -Recurse -Filter "*.snippet")
        {
            $dstFile = Join-Path $snippetDir $srcFile.FullName.Substring($sourceDir.Length)
            $dstDir = Split-Path $dstFile -Parent
            New-Item $dstDir -Type Directory -Force > $null
            Copy-Item -Force -Path $srcFile.FullName -Destination $dstFile
            Write-Host "Copied snippet to $dstFile"
        }
    }
    else
    {
        Write-Host "$snippetDir DOES NOT EXIST - snippets not copied."
    }
}
