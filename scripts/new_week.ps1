param(
  [Parameter(Mandatory=$true)]
  [string]$ReportDate
)

# Parse date
$dt = [DateTime]::Parse($ReportDate)
# Weeks start on Sunday; reporting week is prior week
$weekStart = $dt.AddDays(-((([int]$dt.DayOfWeek + 6) % 7) + 1))
$weekEnd = $weekStart.AddDays(6)
$slug = $dt.ToString('yyyy-MM-dd')

$postDir = Join-Path -Path "posts" -ChildPath $slug
$dataDir = Join-Path -Path $postDir -ChildPath "data"

New-Item -ItemType Directory -Force -Path $postDir | Out-Null
New-Item -ItemType Directory -Force -Path $dataDir | Out-Null

# Copy template
$templatePath = "templates/post.qmd"
$postFile = Join-Path -Path $postDir -ChildPath "index.qmd"
$template = Get-Content $templatePath -Raw
$template = $template -replace "__REPORT_DATE__", $dt.ToString('yyyy-MM-dd')
$template = $template -replace "__WEEK_START__", $weekStart.ToString('yyyy-MM-dd')
$template = $template -replace "__WEEK_END__", $weekEnd.ToString('yyyy-MM-dd')
$template = $template -replace "__DATA_PATH__", "data"
Set-Content -Path $postFile -Value $template

# Create placeholder data files
Set-Content -Path (Join-Path $dataDir "cad.csv") -Value ""
Set-Content -Path (Join-Path $dataDir "phone.csv") -Value ""
Set-Content -Path (Join-Path $dataDir "notes.md") -Value ""

Write-Host "New weekly post scaffolded at $postDir"

# Attempt to copy canonical snapshots from data-archive
$year = $weekStart.ToString('yyyy')
$weekNumber = (Get-Culture).Calendar.GetWeekOfYear($weekStart, [System.Globalization.CalendarWeekRule]::FirstDay, [DayOfWeek]::Sunday)
$archiveDir = Join-Path -Path "data-archive" -ChildPath (Join-Path $year $weekNumber)
if (Test-Path $archiveDir) {
  Write-Host "Found archive at $archiveDir, copying snapshots..."
  Get-ChildItem -Path $archiveDir -File | ForEach-Object {
    Copy-Item -Path $_.FullName -Destination $dataDir -Force
  }
}
