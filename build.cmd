@echo off
cls

.paket\paket.bootstrapper.exe
if errorlevel 1 (
  echo "Downlod paket.bootstrapper.exe from https://github.com/fsprojects/Paket/releases and save it to .paket forlder in solution folder"
  exit /b %errorlevel%
)

.paket\paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

packages\FAKE\tools\FAKE.exe build.fsx %*
