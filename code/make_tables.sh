Rscript code/regressions.R
lualatex -output-dir=output output/Tab_2-mainresults.tex 
lualatex -output-dir=output output/Tab_3-robustness.tex 
lualatex -output-dir=output output/Tab_2-mainresults_orig.tex 
lualatex -output-dir=output output/Tab_3-robustness_orig.tex 
rm -r output/*.aux
rm -r output/*.log
rm -r output/*.sta