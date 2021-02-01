rm(list = ls()) #clear list

#automatic installation of required packages
packages <- c("xlsx","calibrate","stargazer","sandwich","lmtest","getopt","CausalGAM","ggplot2","reshape2","xts",
              "lattice","gridExtra","gtable","plm","lfe","lmtest","car","tis","foreign","MASS","quantreg","ggrepel",
              "dplyr","stringr","datasets","rio","psych","systemfit","MatchIt","CRTgeeDR","eurostat","plyr","zoo","ggthemes",
              "robumeta","metafor","dplyr","clubSandwich","Hmisc","metafor","pracma","pkgs","broom","sjPlot", "here", "data.table")
ipak(packages)

library(rlang)
library(margins)
library(tidyr)
library(xlsx) #Excel-Paket laden
library(calibrate) #Laden des Pakets, das f??r Datenbeschriftung n??tig ist
library (stargazer) #Laden des Pakets, mit dem R-Regressionsoutput in Latex-Tabellen ??bergef??hrt werden kann
library(sandwich)
library(lmtest)
library(getopt)
library(CausalGAM)
library(ggplot2)
library(reshape2)
library(xts)
library(lattice)
library(gridExtra)
library(gtable)
library(plm)
library(lfe)
library(lmtest)
library(car)
library(foreign)
library(MASS)
library(quantreg)
library(ggrepel)
library(dplyr)
library(stringr)
library(ggplot2)
library(datasets)
library(rio)
library(psych)
library(systemfit)
library(foreign)
library(MatchIt)
library(CRTgeeDR)
library(plyr)
library(zoo)
library(ggthemes)
library(readr)
library(countrycode)
library(devtools)
library(wid)
library(sjPlot)
library(gridExtra)
library(here)
library(data.table)

#read openness data

data_reg <- data.table::fread(here::here("data/ECI-growth-data_final.csv"))
data_reg$country <- countrycode(data_reg$ccode, 'iso3c', 'country.name') #convert OECD name codes to three letter country codes (iso3n)
#write.csv(data_reg, "Global_data_extended.csv")

#rescaling variables, taking logs and calculating growth rates
data_reg$Penn_GDP_PPP <- data_reg$Penn_GDP_PPP * 1000000000
data_reg$Penn_GDP_PPP_log <- log(data_reg$Penn_GDP_PPP)
data_reg$log_pop <- log(data_reg$population)
data_reg$oil_exports_share_country <- data_reg$oil_exports_share_country*100
data_reg$coal_and_metal_exports_share_country <- data_reg$oil_exports_share_country*100

data_reg <- plyr::ddply(data_reg,"ccode", transform,
                   pop_growth=c(NA,diff(log_pop)*100)) #calculate population growth

#Dummy variables for high als low income countries
data_reg$HighIncome <- ifelse(as.numeric(data_reg$GNIpc) > 12055, 1,0)
data_reg$LowIncome <- ifelse(as.numeric(data_reg$GNIpc) < 995.99, 1,0)
data_reg$LowerMiddleIncome <- ifelse(data_reg$GNIpc > 996 & data_reg$GNIpc < 3895, 1,0)
data_reg$HigherMiddleIncome <- ifelse(data_reg$GNIpc > 3896 & data_reg$GNIpc < 12055, 1,0)

#1985-2014
data_reg_1962_1984 <- subset(data_reg, Year %in% c('1962', '1963', '1964', '1965', '1966', '1967', '1968', '1969', '1970', '1971', '1972', '1973', '1974', '1975', '1976', '1977', '1978', '1979', '1980', '1981', '1982', '1983', '1984'))
data_reg_1970_1984 <- subset(data_reg, Year %in% c('1970', '1971', '1972', '1973', '1974', '1975', '1976', '1977', '1978', '1979', '1980', '1981', '1982', '1983', '1984'))
data_reg_1985_2014 <- subset(data_reg, Year %in% c('1985', '1986', '1987', '1988', '1989', '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014'))
data_reg_1995_2014 <- subset(data_reg, Year %in% c('1995', '1996', '1997', '1998', '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014'))
data_reg_1990_2010 <- subset(data_reg, Year %in% c('1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010'))
data_reg_1985_1999 <- subset(data_reg, Year %in% c('1985', '1986', '1987', '1988', '1989', '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999'))
data_reg_2000_2014 <- subset(data_reg, Year %in% c('2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014'))
data_reg_1985_1989 <- subset(data_reg, Year %in% c('1985', '1986', '1987', '1988', '1989'))
data_reg_1990_1994 <- subset(data_reg, Year %in% c('1990', '1991', '1992', '1993', '1994'))
data_reg_1995_1999 <- subset(data_reg, Year %in% c('1995', '1996', '1997', '1998', '1999'))
data_reg_2000_2004 <- subset(data_reg, Year %in% c('2000', '2001', '2002', '2003', '2004'))
data_reg_2005_2009 <- subset(data_reg, Year %in% c('2005', '2006', '2007', '2008', '2009'))
data_reg_2010_2014 <- subset(data_reg, Year %in% c('2010', '2011', '2012', '2013', '2014'))

#Subsets for different time periods
data_reg_1962_1984_sub <- subset(data_reg_1962_1984, ccode %in% c('ABW', 'AFG', 'AGO', 'ALB', 'AND', 'ARB', 'ARE', 'ARG', 'ARM', 'ASM', 'ATG', 'AUS', 'AUT', 'AZE', 'BDI', 'BEL', 'BEN', 'BFA', 'BGD', 'BGR', 'BHR', 'BHS', 'BIH', 'BLR', 'BLZ',
                                                        'BOL', 'BRA', 'BRB', 'BRN', 'BTN', 'BWA', 'CAF', 'CAN', 'CEB', 'CHE', 'CHI', 'CHL', 'CHN', 'CIV', 'CMR', 'COD', 'COG', 'COL', 'COM', 'CPV', 'CRI', 'CSS', 'CUB', 'CUW', 'CYM',
                                                        'CYP', 'CZE', 'DEU', 'DJI', 'DMA', 'DNK', 'DOM', 'DZA', 'EAP', 'EAR', 'EAS', 'ECA', 'ECS', 'ECU', 'EGY', 'EMU', 'ERI', 'ESP', 'EST', 'ETH', 'EUU', 'FCS', 'FIN', 'FJI', 'FRA',
                                                        'FRO', 'FSM', 'GAB', 'GBR', 'GEO', 'GHA', 'GIB', 'GIN', 'GMB', 'GNB', 'GNQ', 'GRC', 'GRD', 'GRL', 'GTM', 'GUM', 'GUY', 'HIC', 'HKG', 'HND', 'HPC', 'HRV', 'HTI', 'HUN', 'IBD',
                                                        'IBT', 'IDA', 'IDB', 'IDN', 'IDX', 'IMN', 'IND', 'INX', 'IRL', 'IRN', 'IRQ', 'ISL', 'ISR', 'ITA', 'JAM', 'JOR', 'JPN', 'KAZ', 'KEN', 'KGZ', 'KHM', 'KIR', 'KNA', 'KOR', 'KWT',
                                                        'LAC', 'LAO', 'LBN', 'LBR', 'LBY', 'LCA', 'LCN', 'LDC', 'LIC', 'LIE', 'LKA', 'LMC', 'LMY', 'LSO', 'LTE', 'LTU', 'LUX', 'LVA', 'MAC', 'MAF', 'MAR', 'MCO', 'MDA', 'MDG', 'MDV',
                                                        'MEA', 'MEX', 'MHL', 'MIC', 'MKD', 'MLI', 'MLT', 'MMR', 'MNA', 'MNE', 'MNG', 'MNP', 'MOZ', 'MRT', 'MUS', 'MWI', 'MYS', 'NAC', 'NAM', 'NCL', 'NER', 'NGA', 'NIC', 'NLD', 'NOR',
                                                        'NPL', 'NRU', 'NZL', 'OED', 'OMN', 'OSS', 'PAK', 'PAN', 'PER', 'PHL', 'PLW', 'PNG', 'POL', 'PRE', 'PRI', 'PRK', 'PRT', 'PRY', 'PSE', 'PSS', 'PST', 'PYF', 'QAT', 'ROU', 'RUS',
                                                        'RWA', 'SAS', 'SAU', 'SDN', 'SEN', 'SGP', 'SLB', 'SLE', 'SLV', 'SMR', 'SOM', 'SRB', 'SSA', 'SSD', 'SSF', 'SST', 'STP', 'SUR', 'SVK', 'SVN', 'SWE', 'SWZ', 'SXM', 'SYC', 'SYR',
                                                        'TCA', 'TCD', 'TEA', 'TEC', 'TGO', 'THA', 'TJK', 'TKM', 'TLA', 'TLS', 'TMN', 'TON', 'TSA', 'TSS', 'TTO', 'TUN', 'TUR', 'TUV', 'TWN', 'TZA', 'UGA', 'UKR', 'UMC', 'URY', 'USA',
                                                        'UZB', 'VCT', 'VEN', 'VGB', 'VIR', 'VNM', 'VUT', 'WLD', 'WSM', 'XKX', 'YEM', 'ZAF', 'ZMB', 'ZWE'))


data_reg_1962_1984_AVG <- data_reg_1962_1984_sub %>% 
  dplyr::group_by(ccode) %>% 
  dplyr::summarize(
    eci_Mean = mean(eci, na.rm=TRUE), GDP_pc_growth_Mean = mean(GDP_pc_growth, na.rm=TRUE), Penn_GDP_PPP_log_Mean = mean(Penn_GDP_PPP_log, na.rm=TRUE), KOF_econ_Mean = mean(KOF_econ, na.rm=TRUE), pop_growth_Mean = mean(pop_growth, na.rm=TRUE), hc_Mean = mean(hc, na.rm=TRUE))

data_reg_1970_1984_AVG <- data_reg_1970_1984 %>% 
  dplyr::group_by(ccode) %>% 
  dplyr::summarize(
    eci_Mean = mean(eci, na.rm=TRUE), GDP_pc_growth_Mean = mean(GDP_pc_growth, na.rm=TRUE), Penn_GDP_PPP_log_Mean = mean(Penn_GDP_PPP_log, na.rm=TRUE), KOF_econ_Mean = mean(KOF_econ, na.rm=TRUE), pop_growth_Mean = mean(pop_growth, na.rm=TRUE), hc_Mean = mean(hc, na.rm=TRUE))

data_reg_1985_2014_AVG <- data_reg_1985_2014 %>% 
  dplyr::group_by(ccode) %>% 
  dplyr::summarize(
    eci_Mean = mean(eci, na.rm=TRUE), GDP_pc_growth_Mean = mean(GDP_pc_growth, na.rm=TRUE), Penn_GDP_PPP_log_Mean = mean(Penn_GDP_PPP_log, na.rm=TRUE), KOF_econ_Mean = mean(KOF_econ, na.rm=TRUE), pop_growth_Mean = mean(pop_growth, na.rm=TRUE), hc_Mean = mean(hc, na.rm=TRUE), primaryexports_Mean = mean(primary_exports_1_share_country, na.rm=TRUE), oilexports_Mean = mean(oil_exports_share_country, na.rm=TRUE), coalandmetalexports_Mean = mean(coal_and_metal_exports_share_country, na.rm=TRUE))

data_reg_1990_2010_AVG <- data_reg_1990_2010 %>% 
  dplyr::group_by(ccode) %>% 
  dplyr::summarize(
    eci_Mean = mean(eci, na.rm=TRUE), GDP_pc_growth_Mean = mean(GDP_pc_growth, na.rm=TRUE), Penn_GDP_PPP_log_Mean = mean(Penn_GDP_PPP_log, na.rm=TRUE), KOF_econ_Mean = mean(KOF_econ, na.rm=TRUE), pop_growth_Mean = mean(pop_growth, na.rm=TRUE), hc_Mean = mean(hc, na.rm=TRUE), legalquality_Mean = mean(legal_rel, na.rm=TRUE), politicalquality_Mean = mean(political_rel, na.rm=TRUE), economicquality_Mean = mean(economic_rel, na.rm=TRUE), oilexports_Mean = mean(oil_exports_share_country, na.rm=TRUE), PropertyRights_Mean = mean(PropertyRights, na.rm=TRUE))

data_reg_1985_1999_AVG <- data_reg_1985_1999 %>% 
  group_by(ccode) %>% 
  dplyr::summarize(
    eci_Mean = mean(eci, na.rm=TRUE), GDP_pc_growth_Mean = mean(GDP_pc_growth, na.rm=TRUE), Penn_GDP_PPP_log_Mean = mean(Penn_GDP_PPP_log, na.rm=TRUE), KOF_econ_Mean = mean(KOF_econ, na.rm=TRUE), pop_growth_Mean = mean(pop_growth, na.rm=TRUE), hc_Mean = mean(hc, na.rm=TRUE))

data_reg_2000_2014_AVG <- data_reg_2000_2014 %>% 
  group_by(ccode) %>% 
  dplyr::summarize(
    eci_Mean = mean(eci, na.rm=TRUE), GDP_pc_growth_Mean = mean(GDP_pc_growth, na.rm=TRUE), Penn_GDP_PPP_log_Mean = mean(Penn_GDP_PPP_log, na.rm=TRUE), KOF_econ_Mean = mean(KOF_econ, na.rm=TRUE), pop_growth_Mean = mean(pop_growth, na.rm=TRUE), hc_Mean = mean(hc, na.rm=TRUE))

data_reg_1985_1989_AVG <- data_reg_1985_1989 %>% 
  group_by(ccode) %>% 
  dplyr::summarize(
    eci_Mean = mean(eci, na.rm=TRUE), GDP_pc_growth_Mean = mean(GDP_pc_growth, na.rm=TRUE), Penn_GDP_PPP_log_Mean = mean(Penn_GDP_PPP_log, na.rm=TRUE), KOF_econ_Mean = mean(KOF_econ, na.rm=TRUE), pop_growth_Mean = mean(pop_growth, na.rm=TRUE), hc_Mean = mean(hc, na.rm=TRUE))

data_reg_1990_1994_AVG <- data_reg_1990_1994 %>% 
  group_by(ccode) %>% 
  dplyr::summarize(
    eci_Mean = mean(eci, na.rm=TRUE), GDP_pc_growth_Mean = mean(GDP_pc_growth, na.rm=TRUE), Penn_GDP_PPP_log_Mean = mean(Penn_GDP_PPP_log, na.rm=TRUE), KOF_econ_Mean = mean(KOF_econ, na.rm=TRUE), pop_growth_Mean = mean(pop_growth, na.rm=TRUE), hc_Mean = mean(hc, na.rm=TRUE))

data_reg_1995_1999_AVG <- data_reg_1995_1999 %>% 
  group_by(ccode) %>% 
  dplyr::summarize(
    eci_Mean = mean(eci, na.rm=TRUE), GDP_pc_growth_Mean = mean(GDP_pc_growth, na.rm=TRUE), Penn_GDP_PPP_log_Mean = mean(Penn_GDP_PPP_log, na.rm=TRUE), KOF_econ_Mean = mean(KOF_econ, na.rm=TRUE), pop_growth_Mean = mean(pop_growth, na.rm=TRUE), hc_Mean = mean(hc, na.rm=TRUE))

data_reg_2000_2004_AVG <- data_reg_2000_2004 %>% 
  group_by(ccode) %>% 
  dplyr::summarize(
    eci_Mean = mean(eci, na.rm=TRUE), GDP_pc_growth_Mean = mean(GDP_pc_growth, na.rm=TRUE), Penn_GDP_PPP_log_Mean = mean(Penn_GDP_PPP_log, na.rm=TRUE), KOF_econ_Mean = mean(KOF_econ, na.rm=TRUE), pop_growth_Mean = mean(pop_growth, na.rm=TRUE), hc_Mean = mean(hc, na.rm=TRUE))

data_reg_2005_2009_AVG <- data_reg_2005_2009 %>% 
  group_by(ccode) %>% 
  dplyr::summarize(
    eci_Mean = mean(eci, na.rm=TRUE), GDP_pc_growth_Mean = mean(GDP_pc_growth, na.rm=TRUE), Penn_GDP_PPP_log_Mean = mean(Penn_GDP_PPP_log, na.rm=TRUE), KOF_econ_Mean = mean(KOF_econ, na.rm=TRUE), pop_growth_Mean = mean(pop_growth, na.rm=TRUE), hc_Mean = mean(hc, na.rm=TRUE))

data_reg_2010_2014_AVG <- data_reg_2010_2014 %>% 
  group_by(ccode) %>% 
  dplyr::summarize(
    eci_Mean = mean(eci, na.rm=TRUE), GDP_pc_growth_Mean = mean(GDP_pc_growth, na.rm=TRUE), Penn_GDP_PPP_log_Mean = mean(Penn_GDP_PPP_log, na.rm=TRUE), KOF_econ_Mean = mean(KOF_econ, na.rm=TRUE), pop_growth_Mean = mean(pop_growth, na.rm=TRUE), hc_Mean = mean(hc, na.rm=TRUE))

#cross-section year 1962
data_reg_sub <- select(data_reg, Year, ccode, eci, Penn_GDP_PPP_log, GDP_pc_growth, AdvancedCountry, HighIncome, LowIncome, LowerMiddleIncome, HigherMiddleIncome, pop_growth, hc, OPECdummy)
data_reg_1962 <- subset(data_reg_sub, Year %in% c('1962'))
#data_reg_1962$ccode
#cross-section year 1970
data_reg_1970 <- subset(data_reg, Year %in% c('1970'))

#cross-section year 1984
data_reg_1984 <- subset(data_reg, Year %in% c('1984'))

data_reg_1995 <- subset(data_reg, Year %in% c('1995'))

data_reg_1984_sub <- subset(data_reg_1984, ccode %in% c('ABW', 'AFG', 'AGO', 'ALB', 'AND', 'ARB', 'ARE', 'ARG', 'ARM', 'ASM', 'ATG', 'AUS', 'AUT', 'AZE', 'BDI', 'BEL', 'BEN', 'BFA', 'BGD', 'BGR', 'BHR', 'BHS', 'BIH', 'BLR', 'BLZ',
 'BOL', 'BRA', 'BRB', 'BRN', 'BTN', 'BWA', 'CAF', 'CAN', 'CEB', 'CHE', 'CHI', 'CHL', 'CHN', 'CIV', 'CMR', 'COD', 'COG', 'COL', 'COM', 'CPV', 'CRI', 'CSS', 'CUB', 'CUW', 'CYM',
 'CYP', 'CZE', 'DEU', 'DJI', 'DMA', 'DNK', 'DOM', 'DZA', 'EAP', 'EAR', 'EAS', 'ECA', 'ECS', 'ECU', 'EGY', 'EMU', 'ERI', 'ESP', 'EST', 'ETH', 'EUU', 'FCS', 'FIN', 'FJI', 'FRA',
 'FRO', 'FSM', 'GAB', 'GBR', 'GEO', 'GHA', 'GIB', 'GIN', 'GMB', 'GNB', 'GNQ', 'GRC', 'GRD', 'GRL', 'GTM', 'GUM', 'GUY', 'HIC', 'HKG', 'HND', 'HPC', 'HRV', 'HTI', 'HUN', 'IBD',
 'IBT', 'IDA', 'IDB', 'IDN', 'IDX', 'IMN', 'IND', 'INX', 'IRL', 'IRN', 'IRQ', 'ISL', 'ISR', 'ITA', 'JAM', 'JOR', 'JPN', 'KAZ', 'KEN', 'KGZ', 'KHM', 'KIR', 'KNA', 'KOR', 'KWT',
 'LAC', 'LAO', 'LBN', 'LBR', 'LBY', 'LCA', 'LCN', 'LDC', 'LIC', 'LIE', 'LKA', 'LMC', 'LMY', 'LSO', 'LTE', 'LTU', 'LUX', 'LVA', 'MAC', 'MAF', 'MAR', 'MCO', 'MDA', 'MDG', 'MDV',
 'MEA', 'MEX', 'MHL', 'MIC', 'MKD', 'MLI', 'MLT', 'MMR', 'MNA', 'MNE', 'MNG', 'MNP', 'MOZ', 'MRT', 'MUS', 'MWI', 'MYS', 'NAC', 'NAM', 'NCL', 'NER', 'NGA', 'NIC', 'NLD', 'NOR',
 'NPL', 'NRU', 'NZL', 'OED', 'OMN', 'OSS', 'PAK', 'PAN', 'PER', 'PHL', 'PLW', 'PNG', 'POL', 'PRE', 'PRI', 'PRK', 'PRT', 'PRY', 'PSE', 'PSS', 'PST', 'PYF', 'QAT', 'ROU', 'RUS',
 'RWA', 'SAS', 'SAU', 'SDN', 'SEN', 'SGP', 'SLB', 'SLE', 'SLV', 'SMR', 'SOM', 'SRB', 'SSA', 'SSD', 'SSF', 'SST', 'STP', 'SUR', 'SVK', 'SVN', 'SWE', 'SWZ', 'SXM', 'SYC', 'SYR',
 'TCA', 'TCD', 'TEA', 'TEC', 'TGO', 'THA', 'TJK', 'TKM', 'TLA', 'TLS', 'TMN', 'TON', 'TSA', 'TSS', 'TTO', 'TUN', 'TUR', 'TUV', 'TWN', 'TZA', 'UGA', 'UKR', 'UMC', 'URY', 'USA',
 'UZB', 'VCT', 'VEN', 'VGB', 'VIR', 'VNM', 'VUT', 'WLD', 'WSM', 'XKX', 'YEM', 'ZAF', 'ZMB', 'ZWE'))

#cross-section year 1985
data_reg_1985 <- subset(data_reg, Year %in% c('1985'))

#cross-section year 1990
data_reg_1990 <- subset(data_reg, Year %in% c('1990')) #problem: number of estimates in 1990 is one higher than in other years

#cross-section year 1995
data_reg_1995 <- subset(data_reg, Year %in% c('1995'))

#cross-section year 2000
data_reg_2000 <- subset(data_reg, Year %in% c('2000'))

#cross-section year 2005
data_reg_2005 <- subset(data_reg, Year %in% c('2005'))

#cross-section year 2010
data_reg_2010 <- subset(data_reg, Year %in% c('2010'))

#cross-section year 2014
data_reg_2014 <- subset(data_reg, Year %in% c('2014'))

#eci for 1985 and 2014
data_reg_1985_and_2014 <- data.frame(data_reg_1985$ccode, data_reg_1985$Year, data_reg_1985$eci, data_reg_2014$eci, data_reg_2014$eci, data_reg_1985$AdvancedCountry, data_reg_2014$AdvancedCountry, data_reg_1985$HighIncome, data_reg_2014$HighIncome, data_reg_1985$LowIncome, data_reg_2014$LowIncome, data_reg_1985$LowerMiddleIncome, data_reg_2014$LowerMiddleIncome, data_reg_1985$HigherMiddleIncome, data_reg_1985$pop_growth, data_reg_2014$pop_growth,  data_reg_1985$hc, data_reg_2014$hc,  data_reg_1985$OPECdummy, data_reg_2014$OPECdummy)

#eci and GDP per capita for 1962, growth for average 1962-1984
data_reg_predict_1962_1984 <- data.frame(data_reg_1962$ccode, data_reg_1962$eci, data_reg_1962$Penn_GDP_PPP_log, data_reg_1962_1984_AVG$GDP_pc_growth_Mean, data_reg_1962$AdvancedCountry, data_reg_1962$HighIncome, data_reg_1962$LowIncome, data_reg_1962$LowerMiddleIncome, data_reg_1962$HigherMiddleIncome, data_reg_1962$pop_growth, data_reg_1962$hc, data_reg_1962$OPECdummy)
colnames(data_reg_predict_1962_1984) <- c('ccode', 'eci_1962', 'GDP_pc_PPP_log_1962', 'avg_GDP_pc_PPP_growth_1962_1984', 'AdvancedCountry', 'HighIncome', 'LowIncome', 'LowerMiddleIncome', 'HigherMiddleIncome', 'popgrowth', 'humancapital', 'OPECdummy')

#eci and GDP per capita for 1970, growth for average 1970-1984
data_reg_predict_1970_1984 <- data.frame(data_reg_1970$ccode, data_reg_1970$eci, data_reg_1970$Penn_GDP_PPP_log, data_reg_1970_1984_AVG$GDP_pc_growth_Mean, data_reg_1970_1984_AVG$KOF_econ_Mean, data_reg_1970$AdvancedCountry, data_reg_1970$HighIncome, data_reg_1970$LowIncome, data_reg_1970$LowerMiddleIncome, data_reg_1970$HigherMiddleIncome, data_reg_1970_1984_AVG$pop_growth_Mean, data_reg_1970_1984_AVG$hc_Mean, data_reg_1970$OPECdummy)
colnames(data_reg_predict_1970_1984) <- c('ccode', 'eci', 'GDP_pc_PPP_log', 'avg_GDP_pc_PPP_growth', 'kof_econ', 'AdvancedCountry', 'HighIncome', 'LowIncome', 'LowerMiddleIncome', 'HigherMiddleIncome', 'popgrowth', 'humancapital', 'OPECdummy')

#eci and GDP per capita for 1985, growth for average 1985-2014, rest of the variables: 1985-2014
data_reg_predict_1985_2014 <- data.frame(data_reg_1985$ccode, data_reg_1985$eci, data_reg_1985$Penn_GDP_PPP_log, data_reg_1985_2014_AVG$GDP_pc_growth_Mean, data_reg_1985_2014_AVG$KOF_econ_Mean, data_reg_1985$AdvancedCountry, data_reg_1985$HighIncome, data_reg_1985$LowIncome, data_reg_1985$LowerMiddleIncome, data_reg_1985$HigherMiddleIncome, data_reg_1985_2014_AVG$pop_growth_Mean, data_reg_1985_2014_AVG$hc_Mean, data_reg_1985$OPECdummy, data_reg_1985_2014_AVG$primaryexports_Mean, data_reg_1985_2014_AVG$oilexports_Mean, data_reg_1985_2014_AVG$coalandmetalexports_Mean)
colnames(data_reg_predict_1985_2014) <- c('ccode', 'eci', 'GDP_pc_PPP_log', 'avg_GDP_pc_PPP_growth', 'kof_econ', 'AdvancedCountry', 'HighIncome', 'LowIncome', 'LowerMiddleIncome', 'HigherMiddleIncome', 'popgrowth', 'humancapital', 'OPECdummy', 'primaryexports', 'oilexports', 'coalandmetalexports')

#eci and GDP per capita for 1990, growth for average 1990-2010, rest of the variables: 1990-2010
data_reg_predict_1990_2010 <- data.frame(data_reg_1990$ccode, data_reg_1990$eci, data_reg_1990$Penn_GDP_PPP_log, data_reg_1990_2010_AVG$GDP_pc_growth_Mean, data_reg_1990_2010_AVG$KOF_econ_Mean, data_reg_1990$AdvancedCountry, data_reg_1990$HighIncome, data_reg_1990$LowIncome, data_reg_1990$LowerMiddleIncome, data_reg_1990$HigherMiddleIncome, data_reg_1990_2010_AVG$pop_growth_Mean, data_reg_1990_2010_AVG$hc_Mean, data_reg_1990$OPECdummy, data_reg_1990_2010_AVG$legalquality_Mean, data_reg_1990_2010_AVG$politicalquality_Mean, data_reg_1990_2010_AVG$economicquality_Mean, data_reg_1990_2010_AVG$oilexports_Mean, data_reg_1990_2010_AVG$PropertyRights_Mean)
colnames(data_reg_predict_1990_2010) <- c('ccode', 'eci', 'GDP_pc_PPP_log', 'avg_GDP_pc_PPP_growth', 'kof_econ', 'AdvancedCountry', 'HighIncome', 'LowIncome', 'LowerMiddleIncome', 'HigherMiddleIncome', 'popgrowth', 'humancapital', 'OPECdummy', 'legalquality', 'politicalquality', 'economicquality', 'oilexports', 'propertyrights')

#eci, GDP per capita and natural resource variables for 1995, growth for average 1995-2014, rest of the variables: 1995-2014
data_reg_predict_1995_2014 <- data.frame(data_reg_1995$ccode, data_reg_1995$eci, data_reg_1995$Penn_GDP_PPP_log, data_reg_1995_2014_AVG$GDP_pc_growth_Mean, data_reg_1995_2014_AVG$KOF_econ_Mean, data_reg_1995$AdvancedCountry, data_reg_1995$HighIncome, data_reg_1995$LowIncome, data_reg_1995$LowerMiddleIncome, data_reg_1995$HigherMiddleIncome, data_reg_1995_2014_AVG$pop_growth_Mean, data_reg_1995_2014_AVG$hc_Mean, data_reg_1995$OPECdummy, data_reg_1995_2014_AVG$primaryexports_Mean, data_reg_1995_2014_AVG$naturalressourceexports_Mean)
colnames(data_reg_predict_1995_2014) <- c('ccode', 'eci', 'GDP_pc_PPP_log', 'avg_GDP_pc_PPP_growth', 'kof_econ', 'AdvancedCountry', 'HighIncome', 'LowIncome', 'LowerMiddleIncome', 'HigherMiddleIncome', 'popgrowth', 'humancapital', 'OPECdummy', 'primaryexports', 'naturalressourceexports')

#eci and GDP per capita for 1985, growth for average 1985-1999
data_reg_predict_1985_1999 <- data.frame(data_reg_1985$ccode, data_reg_1985$eci, data_reg_1985$Penn_GDP_PPP_log, data_reg_1985_1999_AVG$GDP_pc_growth_Mean, data_reg_1985_1999_AVG$KOF_econ_Mean, data_reg_1985$AdvancedCountry, data_reg_1985$HighIncome, data_reg_1985$LowIncome, data_reg_1985$LowerMiddleIncome, data_reg_1985$HigherMiddleIncome, data_reg_1985_1999_AVG$pop_growth_Mean, data_reg_1985_1999_AVG$hc_Mean, data_reg_1985$OPECdummy)
colnames(data_reg_predict_1985_1999) <- c('ccode', 'eci', 'GDP_pc_PPP_log', 'avg_GDP_pc_PPP_growth', 'kof_econ', 'AdvancedCountry', 'HighIncome', 'LowIncome', 'LowerMiddleIncome', 'HigherMiddleIncome', 'popgrowth', 'humancapital', 'OPECdummy')

#eci and GDP per capita for 2000, growth for average 2000-2014
data_reg_predict_2000_2014 <- data.frame(data_reg_2000$ccode, data_reg_2000$eci, data_reg_2000$Penn_GDP_PPP_log, data_reg_2000_2014_AVG$GDP_pc_growth_Mean, data_reg_2000_2014_AVG$KOF_econ_Mean, data_reg_2000$AdvancedCountry, data_reg_1985$HighIncome, data_reg_1985$LowIncome, data_reg_1985$LowerMiddleIncome, data_reg_1985$HigherMiddleIncome, data_reg_2000_2014_AVG$pop_growth_Mean, data_reg_2000_2014_AVG$hc_Mean, data_reg_2000$OPECdummy)
colnames(data_reg_predict_2000_2014) <- c('ccode', 'eci', 'GDP_pc_PPP_log', 'avg_GDP_pc_PPP_growth', 'kof_econ', 'AdvancedCountry', 'HighIncome', 'LowIncome', 'LowerMiddleIncome', 'HigherMiddleIncome', 'popgrowth', 'humancapital', 'OPECdummy')

#eci and GDP per capita for 1985, growth for average 1985-1989
data_reg_predict_1985_1989 <- data.frame(data_reg_1985$ccode, data_reg_1985$eci, data_reg_1985$Penn_GDP_PPP_log, data_reg_1985_1989_AVG$GDP_pc_growth_Mean, data_reg_1985_1989_AVG$KOF_econ_Mean, data_reg_1985$AdvancedCountry, data_reg_1985$HighIncome, data_reg_1985$LowIncome, data_reg_1985$LowerMiddleIncome, data_reg_1985$HigherMiddleIncome, data_reg_1985_1989_AVG$pop_growth_Mean, data_reg_1985_1989_AVG$hc_Mean, data_reg_1985$OPECdummy)
colnames(data_reg_predict_1985_1989) <- c('ccode', 'eci', 'GDP_pc_PPP_log', 'avg_GDP_pc_PPP_growth', 'kof_econ', 'AdvancedCountry', 'HighIncome', 'LowIncome', 'LowerMiddleIncome', 'HigherMiddleIncome', 'popgrowth', 'humancapital', 'OPECdummy')

#eci and GDP per capita for 1990, growth for average 1990-1994
data_reg_predict_1990_1994 <- data.frame(data_reg_1990$ccode, data_reg_1990$eci, data_reg_1990$Penn_GDP_PPP_log, data_reg_1990_1994_AVG$GDP_pc_growth_Mean, data_reg_1990_1994_AVG$KOF_econ_Mean, data_reg_1990$AdvancedCountry, data_reg_1990$HighIncome, data_reg_1990$LowIncome, data_reg_1990$LowerMiddleIncome, data_reg_1990$HigherMiddleIncome, data_reg_1990_1994_AVG$pop_growth_Mean, data_reg_1990_1994_AVG$hc_Mean, data_reg_1990$OPECdummy)
colnames(data_reg_predict_1990_1994) <- c('ccode', 'eci', 'GDP_pc_PPP_log', 'avg_GDP_pc_PPP_growth', 'kof_econ', 'AdvancedCountry', 'HighIncome', 'LowIncome', 'LowerMiddleIncome', 'HigherMiddleIncome', 'popgrowth', 'humancapital', 'OPECdummy')

#eci and GDP per capita for 1995, growth for average 1995-1999
data_reg_predict_1995_1999 <- data.frame(data_reg_1995$ccode, data_reg_1995$eci, data_reg_1995$Penn_GDP_PPP_log, data_reg_1995_1999_AVG$GDP_pc_growth_Mean, data_reg_1995_1999_AVG$KOF_econ_Mean, data_reg_1995$AdvancedCountry, data_reg_1995$HighIncome, data_reg_1995$LowIncome, data_reg_1995$LowerMiddleIncome, data_reg_1995$HigherMiddleIncome, data_reg_1995_1999_AVG$pop_growth_Mean, data_reg_1995_1999_AVG$hc_Mean, data_reg_1995$OPECdummy)
colnames(data_reg_predict_1995_1999) <- c('ccode', 'eci', 'GDP_pc_PPP_log', 'avg_GDP_pc_PPP_growth', 'kof_econ', 'AdvancedCountry', 'HighIncome', 'LowIncome', 'LowerMiddleIncome', 'HigherMiddleIncome', 'popgrowth', 'humancapital', 'OPECdummy')

#eci and GDP per capita for 2000, growth for average 2000-2004
data_reg_predict_2000_2004 <- data.frame(data_reg_2000$ccode, data_reg_2000$eci, data_reg_2000$Penn_GDP_PPP_log, data_reg_2000_2004_AVG$GDP_pc_growth_Mean, data_reg_2000_2004_AVG$KOF_econ_Mean, data_reg_2000$AdvancedCountry, data_reg_2000$HighIncome, data_reg_2000$LowIncome, data_reg_2000$LowerMiddleIncome, data_reg_2000$HigherMiddleIncome, data_reg_2000_2004_AVG$pop_growth_Mean, data_reg_2000_2004_AVG$hc_Mean, data_reg_2000$OPECdummy)
colnames(data_reg_predict_2000_2004) <- c('ccode', 'eci', 'GDP_pc_PPP_log', 'avg_GDP_pc_PPP_growth', 'kof_econ', 'AdvancedCountry', 'HighIncome', 'LowIncome', 'LowerMiddleIncome', 'HigherMiddleIncome', 'popgrowth', 'humancapital', 'OPECdummy')

#eci and GDP per capita for 2005, growth for average 2005-2009
data_reg_predict_2005_2009 <- data.frame(data_reg_2005$ccode, data_reg_2005$eci, data_reg_2005$Penn_GDP_PPP_log, data_reg_2005_2009_AVG$GDP_pc_growth_Mean, data_reg_2005_2009_AVG$KOF_econ_Mean, data_reg_2005$AdvancedCountry, data_reg_2005$HighIncome, data_reg_2005$LowIncome, data_reg_2005$LowerMiddleIncome, data_reg_2005$HigherMiddleIncome, data_reg_2005_2009_AVG$pop_growth_Mean, data_reg_2005_2009_AVG$hc_Mean, data_reg_2005$OPECdummy)
colnames(data_reg_predict_2005_2009) <- c('ccode', 'eci', 'GDP_pc_PPP_log', 'avg_GDP_pc_PPP_growth', 'kof_econ', 'AdvancedCountry', 'HighIncome', 'LowIncome', 'LowerMiddleIncome', 'HigherMiddleIncome', 'popgrowth', 'humancapital', 'OPECdummy')

#eci and GDP per capita for 2000, growth for average 2000-2004
data_reg_predict_2010_2014 <- data.frame(data_reg_2010$ccode, data_reg_2010$eci, data_reg_2000$Penn_GDP_PPP_log, data_reg_2010_2014_AVG$GDP_pc_growth_Mean, data_reg_2010_2014_AVG$KOF_econ_Mean, data_reg_2010$AdvancedCountry, data_reg_2010$HighIncome, data_reg_2010$LowIncome, data_reg_2010$LowerMiddleIncome, data_reg_2010$HigherMiddleIncome, data_reg_2010_2014_AVG$pop_growth_Mean, data_reg_2010_2014_AVG$hc_Mean, data_reg_2010$OPECdummy)
colnames(data_reg_predict_2010_2014) <- c('ccode', 'eci', 'GDP_pc_PPP_log', 'avg_GDP_pc_PPP_growth', 'kof_econ', 'AdvancedCountry', 'HighIncome', 'LowIncome', 'LowerMiddleIncome', 'HigherMiddleIncome', 'popgrowth', 'humancapital', 'OPECdummy')

#data frame binding 1985-1999 and 2000-2014
data_reg_predict_10year <- rbind(data_reg_predict_1985_1999, data_reg_predict_2000_2014)
colnames(data_reg_predict_10year) <- c('ccode', 'eci', 'GDP_pc_PPP_log', 'avg_GDP_pc_PPP_growth', 'kof_econ', 'AdvancedCountry', 'HighIncome', 'LowIncome', 'LowerMiddleIncome', 'HigherMiddleIncome', 'popgrowth', 'humancapital', 'OPECdummy')

#data frame binding 1985-1999 and 2000-2014
data_reg_predict_5year <- rbind(data_reg_predict_1985_1989, data_reg_predict_1990_1994, data_reg_predict_1995_1999, data_reg_predict_2000_2004, data_reg_predict_2005_2009, data_reg_predict_2010_2014)
colnames(data_reg_predict_5year) <- c('ccode', 'eci', 'GDP_pc_PPP_log', 'avg_GDP_pc_PPP_growth', 'kof_econ', 'AdvancedCountry', 'HighIncome', 'LowIncome', 'LowerMiddleIncome', 'HigherMiddleIncome', 'popgrowth', 'humancapital', 'OPECdummy')

#5-year averages
regression_data_5_year <- plyr::ddply(data_reg_1985_2014, .(ccode, period), numcolwise(mean)) #calculate 5 year averages for those periods
regression_data_5_year <- dplyr::select(regression_data_5_year, ccode, period, eci, Penn_GDP_PPP_log, GDP_pc_growth, KOF_econ, AdvancedCountry, HighIncome, LowIncome, LowerMiddleIncome, HigherMiddleIncome, pop_growth, hc, OPECdummy)
regression_data_annual <- dplyr::select(data_reg_1985_2014, ccode, Year, eci, Penn_GDP_PPP_log, GDP_pc_growth, KOF_econ, AdvancedCountry, HighIncome, LowIncome, LowerMiddleIncome, HigherMiddleIncome, pop_growth, hc, OPECdummy)
colnames(regression_data_5_year) <- c('ccode', 'period', 'eci', 'Penn_GDP_PPP_log', 'GDP_pc_growth', 'kof_econ', 'AdvancedCountry', 'HighIncome', 'LowIncome', 'LowerMiddleIncome', 'HigherMiddleIncome', 'popgrowth', 'humancapital', 'OPECdummy')
colnames(regression_data_annual) <- c('ccode', 'Year', 'eci', 'Penn_GDP_PPP_log', 'GDP_pc_growth', 'kof_econ', 'AdvancedCountry', 'HighIncome', 'LowIncome', 'LowerMiddleIncome', 'HigherMiddleIncome', 'popgrowth', 'humancapital', 'OPECdummy')

#making the data samples consistent, i.e. make sure that all regressions use the sample underlying sample (driven by data availability for all variables)

data_reg_predict_1970_1984 <- subset(data_reg_predict_1970_1984, ccode %in% c('ALB', 'ARG', 'AUS', 'AUT', 'BDI', 'BEL', 'BEN', 'BFA', 'BGR', 'BOL', 'BRA', 'BRB', 'CAF', 'CAN', 'CHE', 'CHL', 'CHN', 'CIV', 'CMR', 'COG', 'COL', 'CRI', 'CYP',
                                                                              'DEU', 'DNK', 'DOM', 'DZA', 'ECU', 'EGY', 'ESP', 'ETH', 'FIN', 'FJI', 'FRA', 'GAB', 'GBR', 'GHA', 'GMB', 'GRC', 'GTM', 'HKG', 'HND', 'HTI', 'HUN', 'IDN', 'IND',
                                                                              'IRL', 'IRQ', 'ISL', 'ISR', 'ITA', 'JAM', 'JOR', 'JPN', 'KEN', 'KHM', 'KOR', 'KWT', 'LAO', 'LBR', 'MAR', 'MDG', 'MDV', 'MEX', 'MLI', 'MLT', 'MMR', 'MNG', 'MRT',
                                                                              'MUS', 'MWI', 'MYS', 'NER', 'NGA', 'NIC', 'NLD', 'NOR', 'NPL', 'NZL', 'PAK', 'PAN', 'PER', 'PHL', 'POL', 'PRT', 'PRY', 'RWA', 'SAU', 'SDN', 'SEN', 'SGP', 'SLE',
                                                                              'SLV', 'SWE', 'SYR', 'TGO', 'THA', 'TTO', 'TUN', 'TUR', 'TZA', 'UGA', 'URY', 'USA', 'VEN', 'VNM', 'ZAF', 'ZMB'))


data_reg_predict_1995_2014 <- subset(data_reg_predict_1995_2014, ccode %in% c('ALB', 'ARG', 'AUS', 'AUT', 'BDI', 'BEL', 'BEN', 'BFA', 'BGR', 'BOL', 'BRA', 'BRB', 'CAF', 'CAN', 'CHE', 'CHL', 'CHN', 'CIV', 'CMR', 'COG', 'COL', 'CRI', 'CYP',
                                                                              'DEU', 'DNK', 'DOM', 'DZA', 'ECU', 'EGY', 'ESP', 'ETH', 'FIN', 'FJI', 'FRA', 'GAB', 'GBR', 'GHA', 'GMB', 'GRC', 'GTM', 'HKG', 'HND', 'HTI', 'HUN', 'IDN', 'IND',
                                                                              'IRL', 'IRQ', 'ISL', 'ISR', 'ITA', 'JAM', 'JOR', 'JPN', 'KEN', 'KHM', 'KOR', 'KWT', 'LAO', 'LBR', 'MAR', 'MDG', 'MDV', 'MEX', 'MLI', 'MLT', 'MMR', 'MNG', 'MRT',
                                                                              'MUS', 'MWI', 'MYS', 'NER', 'NGA', 'NIC', 'NLD', 'NOR', 'NPL', 'NZL', 'PAK', 'PAN', 'PER', 'PHL', 'POL', 'PRT', 'PRY', 'RWA', 'SAU', 'SDN', 'SEN', 'SGP', 'SLE',
                                                                              'SLV', 'SWE', 'SYR', 'TGO', 'THA', 'TTO', 'TUN', 'TUR', 'TZA', 'UGA', 'URY', 'USA', 'VEN', 'VNM', 'ZAF', 'ZMB'))

data_reg_predict_1990_2010 <- subset(data_reg_predict_1990_2010, ccode %in% c('ALB', 'ARG', 'AUS', 'AUT', 'BDI', 'BEL', 'BEN', 'BFA', 'BGR', 'BOL', 'BRA', 'BRB', 'CAF', 'CAN', 'CHE', 'CHL', 'CHN', 'CIV', 'CMR', 'COG', 'COL', 'CRI', 'CYP',
                                                                              'DEU', 'DNK', 'DOM', 'DZA', 'ECU', 'EGY', 'ESP', 'ETH', 'FIN', 'FJI', 'FRA', 'GAB', 'GBR', 'GHA', 'GMB', 'GRC', 'GTM', 'HKG', 'HND', 'HTI', 'HUN', 'IDN', 'IND',
                                                                              'IRL', 'IRQ', 'ISL', 'ISR', 'ITA', 'JAM', 'JOR', 'JPN', 'KEN', 'KHM', 'KOR', 'KWT', 'LAO', 'LBR', 'MAR', 'MDG', 'MDV', 'MEX', 'MLI', 'MLT', 'MMR', 'MNG', 'MRT',
                                                                              'MUS', 'MWI', 'MYS', 'NER', 'NGA', 'NIC', 'NLD', 'NOR', 'NPL', 'NZL', 'PAK', 'PAN', 'PER', 'PHL', 'POL', 'PRT', 'PRY', 'RWA', 'SAU', 'SDN', 'SEN', 'SGP', 'SLE',
                                                                              'SLV', 'SWE', 'SYR', 'TGO', 'THA', 'TTO', 'TUN', 'TUR', 'TZA', 'UGA', 'URY', 'USA', 'VEN', 'VNM', 'ZAF', 'ZMB'))

#including country name
data_reg_predict_1970_1984_select<-select(data_reg_predict_1970_1984, ccode, eci, GDP_pc_PPP_log, avg_GDP_pc_PPP_growth, kof_econ, popgrowth, humancapital)

data_reg_predict_1970_1984_omit <- na.omit(data_reg_predict_1970_1984_select)
data_reg_predict_1970_1984_omit$ccode

data_reg_predict_1985_2014 <- subset(data_reg_predict_1985_2014, ccode %in% c('ALB', 'ARG', 'AUS', 'AUT', 'BDI', 'BEL', 'BEN', 'BFA', 'BGR', 'BOL', 'BRA', 'BRB', 'CAF', 'CAN', 'CHE', 'CHL', 'CHN', 'CIV', 'CMR', 'COG', 'COL', 'CRI', 'CYP',
                                                                              'DEU', 'DNK', 'DOM', 'DZA', 'ECU', 'EGY', 'ESP', 'ETH', 'FIN', 'FJI', 'FRA', 'GAB', 'GBR', 'GHA', 'GMB', 'GRC', 'GTM', 'HKG', 'HND', 'HTI', 'HUN', 'IDN', 'IND',
                                                                              'IRL', 'IRQ', 'ISL', 'ISR', 'ITA', 'JAM', 'JOR', 'JPN', 'KEN', 'KHM', 'KOR', 'KWT', 'LAO', 'LBR', 'MAR', 'MDG', 'MDV', 'MEX', 'MLI', 'MLT', 'MMR', 'MNG', 'MRT',
                                                                              'MUS', 'MWI', 'MYS', 'NER', 'NGA', 'NIC', 'NLD', 'NOR', 'NPL', 'NZL', 'PAK', 'PAN', 'PER', 'PHL', 'POL', 'PRT', 'PRY', 'RWA', 'SAU', 'SDN', 'SEN', 'SGP', 'SLE',
                                                                              'SLV', 'SWE', 'SYR', 'TGO', 'THA', 'TTO', 'TUN', 'TUR', 'TZA', 'UGA', 'URY', 'USA', 'VEN', 'VNM', 'ZAF', 'ZMB'))

#advanced vs. developing countries
data_reg_predict_1985_2014_advanced <- subset(data_reg_predict_1985_2014, AdvancedCountry %in% c('1'))
data_reg_predict_1985_2014_developing <- subset(data_reg_predict_1985_2014, AdvancedCountry %in% c('0'))
data_reg_predict_1985_2014_HighIncome <- subset(data_reg_predict_1985_2014, HighIncome %in% c('1'))
data_reg_predict_1985_2014_LowIncome <- subset(data_reg_predict_1985_2014, LowIncome %in% c('1'))
data_reg_predict_1985_2014_LowerMiddleIncome <- subset(data_reg_predict_1985_2014, LowerMiddleIncome %in% c('1'))
data_reg_predict_1985_2014_HigherMiddleIncome <- subset(data_reg_predict_1985_2014, HigherMiddleIncome %in% c('1'))

#restrict observations
data_reg_predict_10year_advanced <- subset(data_reg_predict_10year, AdvancedCountry %in% c('1'))
data_reg_predict_10year_developing <- subset(data_reg_predict_10year, AdvancedCountry %in% c('0'))
data_reg_predict_10year_HighIncome <- subset(data_reg_predict_10year, HighIncome %in% c('1'))
data_reg_predict_10year_LowIncome <- subset(data_reg_predict_10year, LowIncome %in% c('1'))
data_reg_predict_10year_LowerMiddleIncome <- subset(data_reg_predict_10year, LowerMiddleIncome %in% c('1'))
data_reg_predict_10year_HigherMiddleIncome <- subset(data_reg_predict_10year, HigherMiddleIncome %in% c('1'))

data_reg_predict_5year_advanced <- subset(data_reg_predict_5year, AdvancedCountry %in% c('1'))
data_reg_predict_5year_developing <- subset(data_reg_predict_5year, AdvancedCountry %in% c('0'))
data_reg_predict_5year_HighIncome <- subset(data_reg_predict_5year, HighIncome %in% c('1'))
data_reg_predict_5year_LowIncome <- subset(data_reg_predict_5year, LowIncome %in% c('1'))
data_reg_predict_5year_LowerMiddleIncome <- subset(data_reg_predict_5year, LowerMiddleIncome %in% c('1'))
data_reg_predict_5year_HigherMiddleIncome <- subset(data_reg_predict_5year, HigherMiddleIncome %in% c('1'))

regression_data_5_year_advanced <- subset(regression_data_5_year, AdvancedCountry %in% c('1'))
regression_data_5_year_developing <- subset(regression_data_5_year, AdvancedCountry %in% c('0'))
regression_data_5_year_HighIncome <- subset(regression_data_5_year, HighIncome %in% c('1'))
regression_data_5_year_LowIncome <- subset(regression_data_5_year, LowIncome %in% c('1'))
regression_data_5_year_LowerMiddleIncome <- subset(regression_data_5_year, LowerMiddleIncome %in% c('1'))
regression_data_5_year_HigherMiddleIncome <- subset(regression_data_5_year, HigherMiddleIncome %in% c('1'))

# Save intermediate data-------------------------------------------------------
save(
  data_reg_predict_1985_2014,
  data_reg_predict_1990_2010,
  data_reg_predict_1970_1984,
  regression_data_5_year,
  data_reg_predict_1985_2014_developing, 
  file = here::here("data/intermediate_data_old.Rdata")
)

#Cross-sectional (average 1985-2014) regression: regress log(GDP per capita) on ECI
reg_level_3 <- lm(Penn_GDP_PPP_log_Mean ~ eci_Mean, data=data_reg_1985_2014_AVG, na.action=na.exclude)
summary(reg_level_3)
coeftest(reg_level_3, vcov=vcovHC)

#Figure 1
plot_reg_level_3 <- ggplot(data=data_reg_1985_2014_AVG,
                           aes(x=eci_Mean, y=Penn_GDP_PPP_log_Mean)) +
  geom_point() +
  geom_smooth(method=lm) +
  xlab("Economic Complexity Index") +
  ylab("GDP per capita (log)") +
  ggtitle("Economic complexity and GDP per capita, average 1985-2014")+
  theme(title=element_text(size=10, face='bold'))+
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.text.x=element_text(size=14))+
  theme(axis.title.x=element_text(size=14)) +
  theme(axis.text.y=element_text(size=14))+
  theme(axis.title.y=element_text(size=14))+
  geom_text(aes(label=ccode),hjust=0, vjust=0)
plot_reg_level_3

#Cross-sectional regression: regress growth in GDP per capita 1985-2014 on initial level of ECI and GDP per capita in 1985
reg_predict_1 <- lm(avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log, data=data_reg_predict_1985_2014, na.action=na.exclude)
summary(reg_predict_1)
coeftest(reg_predict_1, vcov=vcovHC)

###
#include eci
reg_predict_2 <- lm(avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + eci, data=data_reg_predict_1985_2014, na.action=na.exclude)
summary(reg_predict_2)
coeftest(reg_predict_2, vcov=vcovHC)

#include kof econ
reg_predict_3 <- lm(avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + eci + kof_econ, data=data_reg_predict_1985_2014, na.action=na.exclude)
summary(reg_predict_3)
coeftest(reg_predict_3, vcov=vcovHC)

#eci*GDPpc (without kof econ)
reg_predict_4 <- lm(avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + eci*GDP_pc_PPP_log, data=data_reg_predict_1985_2014, na.action=na.exclude)
summary(reg_predict_4)
coeftest(reg_predict_4, vcov=vcovHC)
#interpretation: the impact of complexity on growth is conditional on the initial level of GDP per capita

#eci*GDPpc (with kof econ)
reg_predict_4_kof <- lm(avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + eci*GDP_pc_PPP_log, data=data_reg_predict_1985_2014, na.action=na.exclude)
summary(reg_predict_4_kof)
coeftest(reg_predict_4_kof, vcov=vcovHC)

#eci*GDPpc (with popgrowth)
reg_predict_4_popgrowth <- lm(avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + popgrowth + eci*GDP_pc_PPP_log, data=data_reg_predict_1985_2014, na.action=na.exclude)
summary(reg_predict_4_popgrowth)
coeftest(reg_predict_4_popgrowth, vcov=vcovHC)

#eci*GDPpc (with humancapital)
reg_predict_4_humancapital <- lm(avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + humancapital + eci*GDP_pc_PPP_log, data=data_reg_predict_1985_2014, na.action=na.exclude)
summary(reg_predict_4_humancapital)
coeftest(reg_predict_4_humancapital, vcov=vcovHC)

#eci*kof econ
reg_predict_6 <- lm(avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + eci*kof_econ, data=data_reg_predict_1985_2014, na.action=na.exclude)
summary(reg_predict_6)
coeftest(reg_predict_6, vcov=vcovHC)

#eci*GDPpc (with kof econ)
reg_predict_7 <- lm(avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + eci*GDP_pc_PPP_log + popgrowth + humancapital, data=data_reg_predict_1985_2014, na.action=na.exclude)
summary(reg_predict_7)
coeftest(reg_predict_7, vcov=vcovHC)

data_reg_predict_1985_2014$predicted <- predict(reg_predict_7)   # Save the predicted values
data_reg_predict_1985_2014$residuals <- residuals(reg_predict_7) # Save the residual values

#eci*GDPpc + oilexports (with kof econ)
reg_predict_7_oilexports <- lm(avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + eci*GDP_pc_PPP_log + oilexports + popgrowth + humancapital, data=data_reg_predict_1985_2014, na.action=na.exclude)
summary(reg_predict_7_oilexports )
coeftest(reg_predict_7_oilexports , vcov=vcovHC)

#eci*GDPpc + coalandmetalexports (with kof econ)
reg_predict_7_coalandmetalexports <- lm(avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + eci*GDP_pc_PPP_log + coalandmetalexports + popgrowth + humancapital, data=data_reg_predict_1985_2014, na.action=na.exclude)
summary(reg_predict_7_coalandmetalexports )
coeftest(reg_predict_7_coalandmetalexports , vcov=vcovHC)

#
#eci*GDPpc + economicquality
reg_predict_7_economicquality <- lm(avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + eci*GDP_pc_PPP_log + economicquality + popgrowth + humancapital, data=data_reg_predict_1990_2010, na.action=na.exclude)
summary(reg_predict_7_economicquality)
coeftest(reg_predict_7_economicquality, vcov=vcovHC)

#eci*GDPpc + politicalquality
reg_predict_7_politicalquality <- lm(avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + eci*GDP_pc_PPP_log + politicalquality + popgrowth + humancapital, data=data_reg_predict_1990_2010, na.action=na.exclude)
summary(reg_predict_7_politicalquality)
coeftest(reg_predict_7_politicalquality, vcov=vcovHC)

#eci*GDPpc + politicalquality + oilexports
reg_predict_7_politicalquality_oilexports <- lm(avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + eci*GDP_pc_PPP_log + politicalquality + popgrowth + humancapital + oilexports, data=data_reg_predict_1990_2010, na.action=na.exclude)
summary(reg_predict_7_politicalquality_oilexports)
coeftest(reg_predict_7_politicalquality_oilexports, vcov=vcovHC)

#eci*GDPpc + property rights
reg_predict_7_propertyrights <- lm(avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + eci*GDP_pc_PPP_log + propertyrights + popgrowth + humancapital, data=data_reg_predict_1990_2010, na.action=na.exclude)
summary(reg_predict_7_propertyrights)
coeftest(reg_predict_7_propertyrights, vcov=vcovHC)

#eci*GDPpc + property rights + oil exports
reg_predict_7_propertyrights_oilexports <- lm(avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + eci*GDP_pc_PPP_log + propertyrights + popgrowth + humancapital + oilexports, data=data_reg_predict_1990_2010, na.action=na.exclude)
summary(reg_predict_7_propertyrights_oilexports)
coeftest(reg_predict_7_propertyrights_oilexports, vcov=vcovHC)

#eci*GDPpc + legalquality
reg_predict_7_legalquality <- lm(avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + eci*GDP_pc_PPP_log + legalquality + popgrowth + humancapital, data=data_reg_predict_1990_2010, na.action=na.exclude)
summary(reg_predict_7_legalquality)
coeftest(reg_predict_7_legalquality, vcov=vcovHC)

#preparations for stargazer table
ses.reg_predict_1 <- list(coeftest(reg_predict_1, vcov.=function(x) vcovHC(x))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_predict_1 <- list(coeftest(reg_predict_1, vcov.=function(x) vcovHC(x))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_predict_1 <- list(coeftest(reg_predict_1, vcov.=function(x) vcovHC(x))[,4]) # heteroskedasticity-robust p-val

ses.reg_predict_2 <- list(coeftest(reg_predict_2, vcov.=function(x) vcovHC(x))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_predict_2 <- list(coeftest(reg_predict_2, vcov.=function(x) vcovHC(x))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_predict_2 <- list(coeftest(reg_predict_2, vcov.=function(x) vcovHC(x))[,4]) # heteroskedasticity-robust p-val

ses.reg_predict_3 <- list(coeftest(reg_predict_3, vcov.=function(x) vcovHC(x))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_predict_3 <- list(coeftest(reg_predict_3, vcov.=function(x) vcovHC(x))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_predict_3 <- list(coeftest(reg_predict_3, vcov.=function(x) vcovHC(x))[,4]) # heteroskedasticity-robust p-val

ses.reg_predict_4 <- list(coeftest(reg_predict_4, vcov.=function(x) vcovHC(x))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_predict_4 <- list(coeftest(reg_predict_4, vcov.=function(x) vcovHC(x))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_predict_4 <- list(coeftest(reg_predict_4, vcov.=function(x) vcovHC(x))[,4]) # heteroskedasticity-robust p-val

ses.reg_predict_4_kof <- list(coeftest(reg_predict_4_kof, vcov.=function(x) vcovHC(x))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_predict_4_kof <- list(coeftest(reg_predict_4_kof, vcov.=function(x) vcovHC(x))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_predict_4_kof <- list(coeftest(reg_predict_4_kof, vcov.=function(x) vcovHC(x))[,4]) # heteroskedasticity-robust p-val

ses.reg_predict_4_popgrowth <- list(coeftest(reg_predict_4_popgrowth, vcov.=function(x) vcovHC(x))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_predict_4_popgrowth <- list(coeftest(reg_predict_4_popgrowth, vcov.=function(x) vcovHC(x))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_predict_4_popgrowth <- list(coeftest(reg_predict_4_popgrowth, vcov.=function(x) vcovHC(x))[,4]) # heteroskedasticity-robust p-val

ses.reg_predict_4_humancapital <- list(coeftest(reg_predict_4_humancapital, vcov.=function(x) vcovHC(x))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_predict_4_humancapital <- list(coeftest(reg_predict_4_humancapital, vcov.=function(x) vcovHC(x))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_predict_4_humancapital <- list(coeftest(reg_predict_4_humancapital, vcov.=function(x) vcovHC(x))[,4]) # heteroskedasticity-robust p-val

ses.reg_predict_6 <- list(coeftest(reg_predict_6, vcov.=function(x) vcovHC(x))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_predict_6 <- list(coeftest(reg_predict_6, vcov.=function(x) vcovHC(x))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_predict_6 <- list(coeftest(reg_predict_6, vcov.=function(x) vcovHC(x))[,4]) # heteroskedasticity-robust p-val

ses.reg_predict_7 <- list(coeftest(reg_predict_7, vcov.=function(x) vcovHC(x))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_predict_7 <- list(coeftest(reg_predict_7, vcov.=function(x) vcovHC(x))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_predict_7 <- list(coeftest(reg_predict_7, vcov.=function(x) vcovHC(x))[,4]) # heteroskedasticity-robust p-val

ses.reg_predict_7_oilexports <- list(coeftest(reg_predict_7_oilexports, vcov.=function(x) vcovHC(x))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_predict_7_oilexports <- list(coeftest(reg_predict_7_oilexports, vcov.=function(x) vcovHC(x))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_predict_7_oilexports <- list(coeftest(reg_predict_7_oilexports, vcov.=function(x) vcovHC(x))[,4]) # heteroskedasticity-robust p-val
pvals.reg_predict_7_oilexports 

ses.reg_predict_7_coalandmetalexports <- list(coeftest(reg_predict_7_coalandmetalexports, vcov.=function(x) vcovHC(x))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_predict_7_coalandmetalexports <- list(coeftest(reg_predict_7_coalandmetalexports, vcov.=function(x) vcovHC(x))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_predict_7_coalandmetalexports <- list(coeftest(reg_predict_7_coalandmetalexports, vcov.=function(x) vcovHC(x))[,4]) # heteroskedasticity-robust p-val

ses.reg_predict_7_legalquality <- list(coeftest(reg_predict_7_legalquality, vcov.=function(x) vcovHC(x))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_predict_7_legalquality <- list(coeftest(reg_predict_7_legalquality, vcov.=function(x) vcovHC(x))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_predict_7_legalquality <- list(coeftest(reg_predict_7_legalquality, vcov.=function(x) vcovHC(x))[,4]) # heteroskedasticity-robust p-val

ses.reg_predict_7_politicalquality <- list(coeftest(reg_predict_7_politicalquality, vcov.=function(x) vcovHC(x))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_predict_7_politicalquality <- list(coeftest(reg_predict_7_politicalquality, vcov.=function(x) vcovHC(x))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_predict_7_politicalquality <- list(coeftest(reg_predict_7_politicalquality, vcov.=function(x) vcovHC(x))[,4]) # heteroskedasticity-robust p-val

ses.reg_predict_7_economicquality <- list(coeftest(reg_predict_7_economicquality, vcov.=function(x) vcovHC(x))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_predict_7_economicquality <- list(coeftest(reg_predict_7_economicquality, vcov.=function(x) vcovHC(x))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_predict_7_economicquality <- list(coeftest(reg_predict_7_economicquality, vcov.=function(x) vcovHC(x))[,4]) # heteroskedasticity-robust p-val

ses.reg_predict_7_politicalquality_oilexports <- list(coeftest(reg_predict_7_politicalquality_oilexports, vcov.=function(x) vcovHC(x))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_predict_7_politicalquality_oilexports <- list(coeftest(reg_predict_7_politicalquality_oilexports, vcov.=function(x) vcovHC(x))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_predict_7_politicalquality_oilexports <- list(coeftest(reg_predict_7_politicalquality_oilexports, vcov.=function(x) vcovHC(x))[,4]) # heteroskedasticity-robust p-val

ses.reg_predict_7_propertyrights <- list(coeftest(reg_predict_7_propertyrights, vcov.=function(x) vcovHC(x))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_predict_7_propertyrights <- list(coeftest(reg_predict_7_propertyrights, vcov.=function(x) vcovHC(x))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_predict_7_propertyrights <- list(coeftest(reg_predict_7_propertyrights, vcov.=function(x) vcovHC(x))[,4]) # heteroskedasticity-robust p-val

ses.reg_predict_7_propertyrights_oilexports <- list(coeftest(reg_predict_7_propertyrights_oilexports, vcov.=function(x) vcovHC(x))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_predict_7_propertyrights_oilexports <- list(coeftest(reg_predict_7_propertyrights_oilexports, vcov.=function(x) vcovHC(x))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_predict_7_propertyrights_oilexports <- list(coeftest(reg_predict_7_propertyrights_oilexports, vcov.=function(x) vcovHC(x))[,4]) # heteroskedasticity-robust p-val

###
#marginal effects analysis

library(margins)

#Figure 2
#note: ci.lvl = ... sets the confidence interval (outer probability or high density probability; see package documentation sjplot)
plot_marginal_effects_all <- plot_model(reg_predict_7, type = "pred", terms = c("GDP_pc_PPP_log", "eci [-2, 0, 2]"), ci.lvl=0.9) + font_size(title = 12) +   theme(panel.background = element_rect(fill = "white")) + xlab("GDP per capita in 1985 (in logs)") + ylab("Predicted average growth in GDP per capita (1985-2014)") +  ggtitle("Predicted values of average GDP per capita growth over 1985-2014") + theme(axis.title.x = element_text(size = 8), axis.text.x = element_text(size = 8), axis.title.y = element_text(size = 8), axis.text.y = element_text(size = 8))
plot_marginal_effects_all

#Figure 3
plot_marginal_effects_all_eci <- plot_model(reg_predict_7, type = "pred", terms = c("eci", "GDP_pc_PPP_log [7, 8.5, 10]"), ci.lvl=0.9) + font_size(title = 12) +   theme(panel.background = element_rect(fill = "white")) + xlab("Economic Complexity Index in 1985") + ylab("Predicted average growth in GDP per capita (1985-2014)") +   ggtitle("Predicted values of average GDP per capita growth over 1985-2014") + theme(axis.title.x = element_text(size = 8), axis.text.x = element_text(size = 8), axis.title.y = element_text(size = 8), axis.text.y = element_text(size = 8) + theme(legend.title="GDPpc"))
plot_marginal_effects_all_eci

#Panel estimates (5-year averages, 1980-2014)
reg_panel_5year_1 <- plm(GDP_pc_growth ~ lag(Penn_GDP_PPP_log, 1), data=regression_data_5_year, model="pooling", na.action=na.exclude)
summary(reg_panel_5year_1)
coeftest(reg_panel_5year_1, vcov.=function(x) vcovHC(x, type="sss"))

reg_panel_5year_2 <- plm(GDP_pc_growth ~ lag(Penn_GDP_PPP_log, 1) + lag(eci, 1), data=regression_data_5_year, index=c("ccode", "period"), model="pooling", na.action=na.exclude)
summary(reg_panel_5year_2)
coeftest(reg_panel_5year_2, vcov.=function(x) vcovHC(x, type="sss"))

reg_panel_5year_3 <- plm(GDP_pc_growth ~ lag(Penn_GDP_PPP_log, 1) + lag(eci, 1) + lag(kof_econ, 1), data=regression_data_5_year, index=c("ccode", "period"), model="pooling", na.action=na.exclude)
summary(reg_panel_5year_3)
coeftest(reg_panel_5year_3, vcov.=function(x) vcovHC(x, type="sss"))

reg_panel_5year_4 <- plm(GDP_pc_growth ~ lag(Penn_GDP_PPP_log, 1) + lag(eci, 1) + lag(eci, 1) * lag(Penn_GDP_PPP_log, 1), data=regression_data_5_year, index=c("ccode", "period"), model="pooling", na.action=na.exclude)
summary(reg_panel_5year_4)
coeftest(reg_panel_5year_4, vcov.=function(x) vcovHC(x, type="sss"))

reg_panel_5year_5 <- plm(GDP_pc_growth ~ lag(Penn_GDP_PPP_log, 1) + lag(eci, 1) + lag(kof_econ, 1) + lag(eci, 1) * lag(Penn_GDP_PPP_log, 1), data=regression_data_5_year, index=c("ccode", "period"), model="pooling", na.action=na.exclude)
summary(reg_panel_5year_5)
coeftest(reg_panel_5year_5, vcov.=function(x) vcovHC(x, type="sss"))

reg_panel_5year_6 <- plm(GDP_pc_growth ~ lag(Penn_GDP_PPP_log, 1) + lag(eci, 1) + lag(kof_econ, 1) + lag(eci, 1) * lag(Penn_GDP_PPP_log, 1), data=regression_data_5_year, index=c("ccode", "period"), model="pooling", na.action=na.exclude)
summary(reg_panel_5year_6)
coeftest(reg_panel_5year_6, vcov.=function(x) vcovHC(x, type="sss"))

#5-year averages
reg_panel_5year_7 <- plm(GDP_pc_growth ~ lag(Penn_GDP_PPP_log, 1) + lag(eci,1) + kof_econ + lag(eci,1) * lag(Penn_GDP_PPP_log,1) + popgrowth + humancapital, data=regression_data_5_year, index=c("ccode", "period"), model="pooling", na.action=na.exclude)
summary(reg_panel_5year_7)
coeftest(reg_panel_5year_7, vcov.=function(x) vcovHC(x, type="sss"))

#country-fixed effects
reg_panel_5year_7_cfe <- plm(GDP_pc_growth ~ lag(Penn_GDP_PPP_log, 1) + lag(eci,1) + kof_econ + lag(eci,1) * lag(Penn_GDP_PPP_log,1) + popgrowth + humancapital, data=regression_data_5_year, index=c("ccode", "period"), model="within", effect="individual", na.action=na.exclude)
summary(reg_panel_5year_7_cfe)
coeftest(reg_panel_5year_7_cfe, vcov.=function(x) vcovHC(x, type="sss"))

#country-fixed effects and time-fixed effects
reg_panel_5year_7_ctfe <- plm(GDP_pc_growth ~ lag(Penn_GDP_PPP_log, 1) + lag(eci,1) + kof_econ + lag(eci,1) * lag(Penn_GDP_PPP_log,1) + popgrowth + humancapital, data=regression_data_5_year, index=c("ccode", "period"), model="within", effect="individual", na.action=na.exclude)
summary(reg_panel_5year_7_ctfe)
coeftest(reg_panel_5year_7_ctfe, vcov.=function(x) vcovHC(x, type="sss"))

#preparations for stargazer table
ses.reg_panel_5year_1 <- list(coeftest(reg_panel_5year_1, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_panel_5year_1 <- list(coeftest(reg_panel_5year_1, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_panel_5year_1 <- list(coeftest(reg_panel_5year_1, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.reg_panel_5year_2 <- list(coeftest(reg_panel_5year_2, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_panel_5year_2 <- list(coeftest(reg_panel_5year_2, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_panel_5year_2 <- list(coeftest(reg_panel_5year_2, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.reg_panel_5year_3 <- list(coeftest(reg_panel_5year_3, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_panel_5year_3 <- list(coeftest(reg_panel_5year_3, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_panel_5year_3 <- list(coeftest(reg_panel_5year_3, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.reg_panel_5year_4 <- list(coeftest(reg_panel_5year_4, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_panel_5year_4 <- list(coeftest(reg_panel_5year_4, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_panel_5year_4 <- list(coeftest(reg_panel_5year_4, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.reg_panel_5year_5 <- list(coeftest(reg_panel_5year_5, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_panel_5year_5 <- list(coeftest(reg_panel_5year_5, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_panel_5year_5 <- list(coeftest(reg_panel_5year_5, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.reg_panel_5year_6 <- list(coeftest(reg_panel_5year_6, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_panel_5year_6 <- list(coeftest(reg_panel_5year_6, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_panel_5year_6 <- list(coeftest(reg_panel_5year_6, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.reg_panel_5year_7 <- list(coeftest(reg_panel_5year_7, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_panel_5year_7 <- list(coeftest(reg_panel_5year_7, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_panel_5year_7 <- list(coeftest(reg_panel_5year_7, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.reg_panel_5year_7_cfe <- list(coeftest(reg_panel_5year_7_cfe, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_panel_5year_7_cfe <- list(coeftest(reg_panel_5year_7_cfe, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_panel_5year_7_cfe <- list(coeftest(reg_panel_5year_7_cfe, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

ses.reg_panel_5year_7_cfe <- list(coeftest(reg_panel_5year_7_cfe, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_panel_5year_7_cfe <- list(coeftest(reg_panel_5year_7_cfe, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_panel_5year_7_cfe <- list(coeftest(reg_panel_5year_7_cfe, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val

#stargazer table for LaTEX
stargazer(reg_panel_5year_1, reg_panel_5year_2, reg_panel_5year_3, reg_panel_5year_4, reg_panel_5year_5, reg_panel_5year_6, t=list(unlist(tvals.reg_panel_5year_1), unlist(tvals.reg_panel_5year_2), unlist(tvals.reg_panel_5year_3), unlist(tvals.reg_panel_5year_4), unlist(tvals.reg_panel_5year_5), unlist(tvals.reg_panel_5year_6)), se=list(unlist(ses.reg_panel_5year_1), unlist(ses.reg_panel_5year_2), unlist(ses.reg_panel_5year_3), unlist(ses.reg_panel_5year_4), unlist(ses.reg_panel_5year_5), unlist(ses.reg_panel_5year_6)), p=list(unlist(pvals.reg_panel_5year_1), unlist(pvals.reg_panel_5year_2), unlist(pvals.reg_panel_5year_3), unlist(pvals.reg_panel_5year_4), unlist(pvals.reg_panel_5year_5), unlist(pvals.reg_panel_5year_6)))

#developing countries
reg_predict_7_developing <- lm(avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + eci*GDP_pc_PPP_log + popgrowth + humancapital, data=data_reg_predict_1985_2014_developing, na.action=na.exclude)
summary(reg_predict_7_developing)
coeftest(reg_predict_7_developing, vcov=vcovHC)

ses.reg_predict_7_developing <- list(coeftest(reg_predict_7_developing, vcov.=function(x) vcovHC(x))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_predict_7_developing <- list(coeftest(reg_predict_7_developing, vcov.=function(x) vcovHC(x))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_predict_7_developing <- list(coeftest(reg_predict_7_developing, vcov.=function(x) vcovHC(x))[,4]) # heteroskedasticity-robust p-val

#1970-184
reg_predict_7_1970_1984 <- lm(avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + eci*GDP_pc_PPP_log + kof_econ + popgrowth + humancapital, data=data_reg_predict_1970_1984, na.action=na.exclude)
summary(reg_predict_7_1970_1984)
coeftest(reg_predict_7_1970_1984, vcov=vcovHC)

ses.reg_predict_7_1970_1984 <- list(coeftest(reg_predict_7_1970_1984, vcov.=function(x) vcovHC(x))[,2]) #heteroskedasticity-robust standard errors
tvals.reg_predict_7_1970_1984 <- list(coeftest(reg_predict_7_1970_1984, vcov.=function(x) vcovHC(x))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
pvals.reg_predict_7_1970_1984 <- list(coeftest(reg_predict_7_1970_1984, vcov.=function(x) vcovHC(x))[,4]) # heteroskedasticity-robust p-val

#Table 2: Main regression results
stargazer(reg_predict_1, reg_predict_4, reg_predict_4_kof, reg_predict_4_popgrowth, reg_predict_4_humancapital, reg_predict_7, reg_panel_5year_7, reg_panel_5year_7_cfe, t=list(unlist(tvals.reg_predict_1), unlist(tvals.reg_predict_4), unlist(tvals.reg_predict_4_kof), unlist(tvals.reg_predict_4_popgrowth), unlist(tvals.reg_predict_4_humancapital), unlist(tvals.reg_predict_7), unlist(tvals.reg_panel_5year_7), unlist(tvals.reg_panel_5year_7_cfe)), se=list(unlist(ses.reg_predict_1), unlist(ses.reg_predict_4), unlist(ses.reg_predict_4_kof), unlist(ses.reg_predict_4_popgrowth), unlist(ses.reg_predict_4_humancapital), unlist(ses.reg_predict_7), unlist(ses.reg_panel_5year_7), unlist(ses.reg_panel_5year_7_cfe)), p=list(unlist(pvals.reg_predict_1), unlist(pvals.reg_predict_4), unlist(pvals.reg_predict_4_kof), unlist(pvals.reg_predict_4_popgrowth), unlist(pvals.reg_predict_4_humancapital), unlist(pvals.reg_predict_7), unlist(pvals.reg_panel_5year_7), unlist(pvals.reg_panel_5year_7_cfe)), float = F, out = here::here("output/tex/Tab2_mainresults_raw_old.tex"))

#Table 3: Robustness checks
stargazer(reg_predict_7, reg_predict_7_developing, reg_predict_7_1970_1984, reg_predict_7_oilexports, reg_predict_7_coalandmetalexports, reg_predict_7_economicquality, reg_predict_7_politicalquality, reg_predict_7_legalquality, reg_predict_7_politicalquality_oilexports, t=list(unlist(tvals.reg_predict_7), unlist(tvals.reg_predict_7_developing), unlist(tvals.reg_predict_7_1970_1984), unlist(tvals.reg_predict_7_oilexports), unlist(tvals.reg_predict_7_coalandmetalexports),  unlist(tvals.reg_predict_7_economicquality), unlist(tvals.reg_predict_7_politicalquality), unlist(tvals.reg_predict_7_legalquality), unlist(tvals.reg_predict_7_politicalquality_oilexports)), se=list(unlist(ses.reg_predict_7), unlist(ses.reg_predict_7_developing), unlist(ses.reg_predict_7_1970_1984), unlist(ses.reg_predict_7_oilexports), unlist(ses.reg_predict_7_coalandmetalexports), unlist(ses.reg_predict_7_economicquality), unlist(ses.reg_predict_7_politicalquality), unlist(ses.reg_predict_7_legalquality), unlist(ses.reg_predict_7_politicalquality_oilexports)), p=list(unlist(pvals.reg_predict_7), unlist(pvals.reg_predict_7_developing), unlist(pvals.reg_predict_7_1970_1984), unlist(pvals.reg_predict_7_oilexports), unlist(pvals.reg_predict_7_coalandmetalexports), unlist(pvals.reg_predict_7_economicquality), unlist(pvals.reg_predict_7_politicalquality), unlist(pvals.reg_predict_7_legalquality), unlist(pvals.reg_predict_7_politicalquality_oilexports)), float = F)

stargazer(reg_panel_5year_7, reg_panel_5year_7_cfe, 
          t=list(unlist(tvals.reg_panel_5year_7), unlist(tvals.reg_panel_5year_7_cfe)),
          se=list(unlist(ses.reg_panel_5year_7), unlist(ses.reg_panel_5year_7_cfe)),
          p = list(unlist(pvals.reg_panel_5year_7), unlist(pvals.reg_panel_5year_7_cfe)),
          float = FALSE, out = here("output/tex/panel_old.tex"),
          dep.var.caption = "", column.labels = c("dplyr (pooled)", "dplyr (within)"),
          digits = 2, dep.var.labels.include = FALSE, 
          model.names = FALSE, omit.stat =  c("ser", "f")
          )

# Appendix

#Descriptive statistics
stargazer(data_reg_predict_1985_2014) #cross-section
stargazer(data_reg_predict_1990_2010) #cross-section
stargazer(regression_data_5_year) #panel, 5-year averages

#Table with property rights
stargazer(reg_predict_7_politicalquality, reg_predict_7_politicalquality_oilexports, reg_predict_7_propertyrights, reg_predict_7_propertyrights_oilexports, t=list(unlist(tvals.reg_predict_7_politicalquality), unlist(tvals.reg_predict_7_politicalquality_oilexports), unlist(tvals.reg_predict_7_propertyrights), unlist(tvals.reg_predict_7_propertyrights_oilexports)), se=list(unlist(ses.reg_predict_7_politicalquality), unlist(ses.reg_predict_7_politicalquality_oilexports), unlist(ses.reg_predict_7_propertyrights), unlist(ses.reg_predict_7_propertyrights_oilexports)), p=list(unlist(pvals.reg_predict_7_politicalquality), unlist(pvals.reg_predict_7_politicalquality_oilexports), unlist(pvals.reg_predict_7_propertyrights), unlist(pvals.reg_predict_7_propertyrights_oilexports)))

#Residual plots and diagnostics
#residuals vs. fitted plot
plot(reg_predict_7, which=1, col=c("blue"))

#Normal Q-Q plot
plot(reg_predict_7, which=2, col=c("red"))
#Residuals should be normally distributed and the Q-Q Plot will show this. If residuals follow close to a straight line on this plot, it is a good indication they are normally distributed.

#test for autocorrelation
dwtest(reg_predict_7)

#correlation matrix

corr_matrix_data <- select(data_reg_predict_1990_2010, eci, GDP_pc_PPP_log, avg_GDP_pc_PPP_growth, kof_econ, popgrowth, humancapital, legalquality, politicalquality, economicquality, oilexports)
colnames(corr_matrix_data) <- c('ECI', 'GDPpc', 'growth', 'global', 'pop', 'hc', 'linst', 'pinst', 'einst', 'oil')

round(cor(corr_matrix_data, method="pearson", use="complete.obs"),
      digits = 2)
