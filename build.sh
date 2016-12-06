#!/bin/bash
if [ -f ".paket/paket.exe" ]
then
   echo "paket.exe is available. So skipping paket.bootstrapper"
else
  bootstrapperURL=$(curl -s https://api.github.com/repos/fsprojects/Paket/releases/latest | jq -r ".assets[] | select(.name | test(\"paket.bootstrapper.exe\")) | .browser_download_url")
  wget $bootstrapperURL -P .paket
  mono .paket/paket.bootstrapper.exe
fi
exit_code=$?
if [ $exit_code -ne 0 ]; then
  exit $exit_code
fi
mono .paket/paket.exe restore
exit_code=$?
if [ $exit_code -ne 0 ]; then
	exit $exit_code
fi
export MONO_MANAGED_WATCHER=false
mono --runtime=v4.0 packages/FAKE/tools/FAKE.exe build.fsx $@