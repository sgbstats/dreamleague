@echo off

rem Set the path to the Rscript executable
set RSCRIPT="C:\Program Files\R\R-4.4.1\bin\Rscript.exe"

rem Set the path to the R script to execute
set RSCRIPT_FILE="C:\R\git\dreamleague\R\dl-preprocessing-run.R"

rem Execute the R script
%RSCRIPT% %RSCRIPT_FILE%


:: Change to the directory of your Git repository
cd /d C:\\R\\git\\dreamleague

:: Add the specific file
set FILE_PATH=dreamleague\\data.RDa
git add "%FILE_PATH%"

:: Commit the changes with a message
set COMMIT_MSG="Updated file"
git commit -m %COMMIT_MSG%

:: Push the changes to the remote repository
set BRANCH=main
git push origin %BRANCH%


rem Pause so the user can see the output
exit