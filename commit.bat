@echo off

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

exit