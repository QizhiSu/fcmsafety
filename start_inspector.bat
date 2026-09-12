@echo off
rem ============================================================
rem  FCMSafety Database Viewer - one-click launcher
rem  Double-click this file to start. The browser will open the
rem  web viewer automatically at http://localhost:3838
rem  To stop the viewer later: close this black window
rem  (or press Ctrl+C inside it).
rem ============================================================

echo.
echo  Starting FCMSafety web viewer, please wait...
echo  First launch loads all code and may take 20-60 seconds.
echo  The browser will open automatically when ready.
echo.
echo  To stop it later: close this black window.
echo.

"C:\Program Files\R\R-4.6.1\bin\Rscript.exe" "C:\Users\13432\WorkBuddy\2026-08-31-14-16-57\fcmsafety\launch_inspector.R"

echo.
echo  The web viewer has stopped.
pause
