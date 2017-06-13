$encoding=[System.Text.Encoding]::GetEncoding('iso-8859-1')

$result = "EKCH.csv"
$csvs = get-childItem "EKCH-Daily-*.csv" 
#read and write CSV header
gc $csvs[0] -Encoding UTF8 | select -First 1 | % { $_.replace(" ", "").replace("Km/h", "KmH") } | Out-File $result -Encoding UTF8
#read and append file contents minus header
foreach ($csv in $csvs)  {
    gc $csv -Encoding UTF8 | select -Skip 1 | Out-File $result -Encoding UTF8 -Append
}

[System.IO.File]::WriteAllLines($result, (gc $result -Encoding UTF8), $encoding)

$result = "EKCH_Aggr.csv"
$csvs = get-childItem "EKCH-Monthly-*.csv" 
#read and write CSV header
gc $csvs[0] -Encoding UTF8 | select -First 1 |  % { $_.replace(" ", "").replace("Km/h", "KmH") } | Out-File $result -Encoding UTF8
#read and append file contents minus header
foreach ($csv in $csvs)  {
    gc $csv -Encoding UTF8 | select -Skip 1 | Out-File $result -Encoding UTF8 -Append
}

[System.IO.File]::WriteAllLines($result, (gc $result -Encoding UTF8), $encoding)