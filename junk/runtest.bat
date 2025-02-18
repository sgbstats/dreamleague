@echo off
SET "R_HOME=C:\Program Files\R\R-4.4.1"
SET "R_SCRIPT=%R_HOME%\bin\Rscript.exe"

"%R_SCRIPT%" -e "renv::activate(); rsconnect::deployApp('dreamleague')"
pause