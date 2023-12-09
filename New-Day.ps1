param(
    [string]$Language,
    [int]$Day = (Get-Date).Day,
    [int]$Year = (Get-Date).Year
)

$LanguageDirectory = "$PSScriptRoot\$Language"
if (-not (Test-Path $LanguageDirectory)) {
    Write-Error "Language directory '$LanguageDirectory' does not exist"
    return
}

$Directory = "$LanguageDirectory\$Year\$($Day.ToString("00"))"
$TemplateDirectory = "$LanguageDirectory\Template"

Copy-Item -Path "$TemplateDirectory" -Destination $Directory -Recurse -ErrorAction SilentlyContinue

$Headers = @{
    "User-Agent" = "github.com-michaeloyer-AdventOfCode-New-Day.ps1"
    "Cookie" = "session=$(Get-Content "$PSScriptRoot\session"))"
}

$InputUrl = "https://adventofcode.com/$Year/day/$Day/input"

Invoke-WebRequest -Uri $InputUrl -Headers $Headers -OutFile "$Directory\puzzle.txt"

$InputUrl = "https://adventofcode.com/$Year/day/$Day"

$html = Invoke-RestMethod -Uri $InputUrl -Headers $Headers

$matches = [Regex]::Matches($html, "<pre><code>([\s\S]+?)</code></pre>")

$matches[0].Groups[1].Value.Trim() > "$Directory\example.txt"

if ($matches.Count -gt 1) {
    $matches[1].Groups[1].Value.Trim() > "$Directory\example2.txt"
}

