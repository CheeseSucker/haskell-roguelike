@echo off
if "%1" == "new-window" (goto NEW_WINDOW)

set LINES=20
set COLUMNS=80
REM ghc -isrc -outputdir bin -o bin/roguelike.exe src/roguelike.hs
REM bin\roguelike.exe
bash run

goto STOP

:NEW_WINDOW
start run.bat

:STOP
exit