@echo off
echo ========================================
echo Starting Shiny App Setup and Launch
echo ========================================
echo.
echo This will:
echo 1. Install required packages (first time only)
echo 2. Launch the Shiny app
echo.
echo Please wait... this may take several minutes on first run.
echo.

REM Find R installation
set "RSCRIPT="
if exist "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" (
    set "RSCRIPT=C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
) else if exist "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" (
    set "RSCRIPT=C:\Program Files\R\R-4.5.1\bin\Rscript.exe"
) else if exist "C:\Program Files\R\R-4.5.0\bin\Rscript.exe" (
    set "RSCRIPT=C:\Program Files\R\R-4.5.0\bin\Rscript.exe"
) else (
    echo ERROR: R installation not found!
    echo Please install R from https://cran.r-project.org/bin/windows/base/
    echo.
    pause
    exit /b 1
)

echo Found R at: %RSCRIPT%
echo.

REM Run the R script
"%RSCRIPT%" run.R

echo.
echo ========================================
echo App has been closed
echo ========================================
pause
