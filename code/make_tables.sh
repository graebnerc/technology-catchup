echo "Run data_setup.R"
Rscript code/data_setup.R > output/log.txt
echo "Run fig_1-ECI-GDP.R"
Rscript code/fig_1-ECI-GDP.R >> output/log.txt
echo "Run regressions.R"
Rscript code/regressions.R >> output/log.txt
echo "Run robustness.R"
Rscript code/robustness.R >> output/log.txt
echo "Run fig_2_3_margins.R"
Rscript code/fig_2_3_margins.R >> output/log.txt

echo "Run lualatex to create tables: Table 2"
lualatex -output-dir=output output/Tab_2-mainresults.tex >> output/log.txt
echo "Run lualatex to create tables: Table 3"
lualatex -output-dir=output output/Tab_3-robustness.tex >> output/log.txt 
echo "Cleaning up..."
rm -r output/*.aux
rm -r output/*.log
rm -r output/*.sta
echo "Complete replication! See output/log.txt for log info."
