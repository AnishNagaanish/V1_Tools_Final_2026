@echo off
echo ========================================
echo PDF Text Inserter Tool
echo ========================================
echo.

REM Check if Python is installed
python --version >nul 2>&1
if errorlevel 1 (
    echo ERROR: Python is not installed or not in PATH
    echo Please install Python from https://www.python.org/
    pause
    exit /b 1
)

echo Python found!
echo.

REM Check if PyMuPDF is installed
python -c "import fitz" >nul 2>&1
if errorlevel 1 (
    echo PyMuPDF library not found. Installing...
    pip install PyMuPDF
    if errorlevel 1 (
        echo ERROR: Failed to install PyMuPDF
        pause
        exit /b 1
    )
    echo PyMuPDF installed successfully!
    echo.
)

echo Starting PDF Text Inserter...
echo.

REM Run the Python script
python pdf_text_inserter.py

if errorlevel 1 (
    echo.
    echo ERROR: Script execution failed
    pause
    exit /b 1
)

echo.
echo ========================================
echo Process completed successfully!
echo ========================================
pause