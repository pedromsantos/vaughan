$latestRelease = Invoke-RestMethod https://api.github.com/repos/fsprojects/Paket/releases/latest?access_token=TSD_GITHUB_TOKEN
$latestReleaseURL = $latestRelease.assets.browser_download_url[0]
Invoke-WebRequest -Uri $latestReleaseURL -OutFile .paket/paket.bootstrapper.exe