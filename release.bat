@echo off
setlocal enabledelayedexpansion

echo ============================================
echo BQC Build Script
echo ============================================

:: Change to script directory
cd /d "%~dp0"

:: ============================================
:: Step 1: Compile Win64 Release
:: ============================================
echo.
echo [1/3] Compiling Win64 Release...

call "C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat"
if errorlevel 1 (
    echo ERROR: Failed to set RAD Studio environment
    exit /b 1
)

msbuild bqc.dproj /t:Build /p:Config=Release /p:Platform=Win64 /v:m /nologo
if errorlevel 1 (
    echo ERROR: Build failed
    exit /b 1
)

echo Build successful.

:: ============================================
:: Step 2: Get git branch and tag
:: ============================================
echo.
echo [2/3] Getting git info...

:: Get current branch name
for /f "tokens=*" %%i in ('git rev-parse --abbrev-ref HEAD 2^>nul') do set BRANCH=%%i

:: If detached HEAD, use short commit hash
if "%BRANCH%"=="HEAD" (
    for /f "tokens=*" %%i in ('git rev-parse --short HEAD 2^>nul') do set BRANCH=%%i
)

:: If still empty, use "unknown"
if "%BRANCH%"=="" set BRANCH=unknown

:: Get current tag (if HEAD is tagged)
set TAG=
for /f "tokens=*" %%i in ('git describe --tags --exact-match HEAD 2^>nul') do set TAG=%%i

:: Build archive name
set ARCHIVE_NAME=bqc_%BRANCH%
if not "%TAG%"=="" set ARCHIVE_NAME=bqc_%BRANCH%_%TAG%

:: Replace invalid filename characters (/ -> -)
set ARCHIVE_NAME=%ARCHIVE_NAME:/=-%

echo Branch: %BRANCH%
if not "%TAG%"=="" (
    echo Tag: %TAG%
) else (
    echo Tag: ^(none^)
)
echo Archive name: %ARCHIVE_NAME%.zip

:: ============================================
:: Step 3: Create ZIP archive
:: ============================================
echo.
echo [3/3] Creating archive...

:: Delete existing archive if present
if exist "%ARCHIVE_NAME%.zip" del "%ARCHIVE_NAME%.zip"

:: Create ZIP using PowerShell
:: Use temp staging folder to ensure correct archive structure
powershell -NoProfile -Command ^
    "$staging = 'build_temp'; " ^
    "$archive = '%ARCHIVE_NAME%.zip'; " ^
    "if (Test-Path $staging) { Remove-Item $staging -Recurse -Force }; " ^
    "New-Item -ItemType Directory -Path $staging | Out-Null; " ^
    "New-Item -ItemType Directory -Path \"$staging\img\" | Out-Null; " ^
    "$filesToCopy = @(" ^
        "@{Src='Win64\Release\bqc.exe'; Dst=\"$staging\bqc.exe\"}, " ^
        "@{Src='bqc.ini'; Dst=\"$staging\bqc.ini\"}, " ^
        "@{Src='README.md'; Dst=\"$staging\README.md\"}, " ^
        "@{Src='LICENSE.txt'; Dst=\"$staging\LICENSE.txt\"}, " ^
        "@{Src='img\bqc.png'; Dst=\"$staging\img\bqc.png\"}" ^
    "); " ^
    "$missing = $filesToCopy | Where-Object { -not (Test-Path $_.Src) } | ForEach-Object { $_.Src }; " ^
    "if ($missing) { Write-Host 'ERROR: Missing files:' $missing -ForegroundColor Red; exit 1 }; " ^
    "$filesToCopy | ForEach-Object { Copy-Item $_.Src $_.Dst }; " ^
    "if (Test-Path $archive) { Remove-Item $archive -Force }; " ^
    "Compress-Archive -Path \"$staging\*\" -DestinationPath $archive; " ^
    "Remove-Item $staging -Recurse -Force; " ^
    "if ($?) { Write-Host 'Archive created successfully.' } else { exit 1 }"

if errorlevel 1 (
    echo ERROR: Failed to create archive
    exit /b 1
)

echo.
echo ============================================
echo Build complete: %ARCHIVE_NAME%.zip
echo ============================================

endlocal
