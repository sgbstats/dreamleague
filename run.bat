@echo off
rem Set the path to Rscript
set RSCRIPT="C:\Program Files\R\R-4.4.1\bin\Rscript.exe"
rem %RSCRIPT% --no-init-file -e "install.packages('curl', type='binary', repos='https://cran.rstudio.com/')"

rem Set the path to your directory
cd C:/R/git/dreamleague


set RSCRIPT_FILE="R\dl-preprocessing-run.R"

%RSCRIPT% --no-init-file %RSCRIPT_FILE%

pause
