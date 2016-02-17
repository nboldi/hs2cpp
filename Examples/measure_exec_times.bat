@echo off

set "startTime=%time%"
set "N=100"
set "diff=0"
for /l %%x in (1, 1, %N%) do (
    gcc -std=c99 -E -P -I. test.c > nul 2>&1
)
set "stopTime=%time%"
call :timeDiff diff startTime stopTime
set /a "diffAvg=%diff%/%N%"
echo %diffAvg%
goto :eof

:timeDiff
setlocal
call :timeToMS time1 "%~2"
call :timeToMS time2 "%~3"
set /a diff=time2-time1
(
  ENDLOCAL
  set "%~1=%diff%"
  goto :eof
)

:timeToMS
::### WARNING, enclose the time in " ", because it can contain comma seperators
SETLOCAL EnableDelayedExpansion
FOR /F "tokens=1,2,3,4 delims=:,.^ " %%a IN ("!%~2!") DO (
  set /a "ms=(((30%%a%%100)*60+7%%b)*60+3%%c-42300)*1000+(1%%d0 %% 1000)"
)
(
  ENDLOCAL
  set %~1=%ms%
  goto :eof
)