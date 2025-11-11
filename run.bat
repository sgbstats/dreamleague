@echo off
rem Set the path to Rscript
set RSCRIPT="C:\Program Files\R\R-4.4.1\bin\Rscript.exe"
rem %RSCRIPT% --no-init-file -e "install.packages('curl', type='binary', repos='https://cran.rstudio.com/')"
cd C:/R/git/dreamleague

rem Set the path to your R script
set RSCRIPT_FILE="C:\R\git\dreamleague\R\dl-preprocessing-run.R"

%RSCRIPT% --no-init-file %RSCRIPT_FILE%

pause
