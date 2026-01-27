@echo off
rem Set the path to Rscript
set RSCRIPT="C:/Users/80010008/AppData/Local/Programs/R/R-4.5.2\bin\Rscript.exe"
rem %RSCRIPT% --no-init-file -e "install.packages('vroom', type='binary', repos='https://cran.rstudio.com/')"

rem Set the path to your directory

cd c:/Users/80010008/git/dreamleague



set RSCRIPT_FILE="R\dl-preprocessing-run.R"

%RSCRIPT% --no-init-file %RSCRIPT_FILE%

pause
