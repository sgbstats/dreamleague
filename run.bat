@echo off

rem Set the path to the Rscript executable
set RSCRIPT="C:\Program Files\R\R-4.4.1\bin\Rscript.exe"

rem Set the path to the R script to execute
set RSCRIPT_FILE="C:\R\git\dreamleague\R\dl-preprocessing-run.R"

rem Execute the R script
%RSCRIPT% %RSCRIPT_FILE%

pause