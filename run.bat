@echo off

rem Set the path to the Rscript executable
set RSCRIPT="C:\Program Files\R\R-4.4.1\bin\Rscript.exe"

rem Set the path to the R script to execute
set RSCRIPT_FILE="C:\R\git\dreamleague\R\dl-preprocessing-run.R"

rem Execute the R script
%RSCRIPT% %RSCRIPT_FILE%


rem Change to the directory of your Git repository
cd /d C:\\R\\git\\dreamleague

rem Add the specific file
rem set FILE_PATH=dreamleague/data.RDa
rem git add "%FILE_PATH%"

rem Commit the changes with a message
rem set COMMIT_MSG="Updated file"
rem git commit -m %COMMIT_MSG%

rem Push the changes to the remote repository
rem set BRANCH=main
rem git push origin %BRANCH%


rem Pause so the user can see the output
pause