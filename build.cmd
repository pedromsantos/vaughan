@echo off
cls

.paket\paket.bootstrapper.exe
if errorlevel 1 (
  mkdir .paket
  powershell -command ./build.ps1
  .paket\paket.bootstrapper.exe
)

.paket\paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

packages\FAKE\tools\FAKE.exe build.fsx %*
