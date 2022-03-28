suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(countrycode))

threshold_high_income <- 12055
threshold_low_income <- 995.99
threshold_lowermiddle_income <- 3895

vars_used <- c(
  "AdvancedCountry", "HighIncome", "LowIncome", "LowerMiddleIncome", 
  "HigherMiddleIncome", "OPECdummy",
  "inv_share" # TODO <- Not yet considered
  )

vars_inst <- c("political_rel", "economic_rel", "legal_rel", "PropertyRights")

vars_used_avg <- c(
  "ccode", 
  "eci", "Penn_GDP_PPP_log","GDP_pc_growth", "KOF_econ", "pop_growth", "hc",
  "primary_exports_1_share_country", "oil_exports_share_country", 
  "coal_and_metal_exports_share_country", "domesticcredit")

total_vars <- c(
  "Year", "Penn_GDP_PPP", "population", "GNIpc", "PropertyRights", "period",
  "propertyrights", "politicalquality", "economicquality", "legalquality",
  vars_used_avg, vars_used, vars_inst)

# making the data samples consistent, i.e. make sure that all regressions use 
# the sample underlying sample (driven by data availability for all variables)

countries_considered <- c(
  'ALB', 'ARG', 'AUS', 'AUT', 'BDI', 'BEL', 'BEN', 'BFA', 'BGR', 'BOL', 'BRA', 
  'BRB', 'CAF', 'CAN', 'CHE', 'CHL', 'CHN', 'CIV', 'CMR', 'COG', 'COL', 'CRI', 
  'CYP', 'DEU', 'DNK', 'DOM', 'DZA', 'ECU', 'EGY', 'ESP', 'ETH', 'FIN', 'FJI', 
  'FRA', 'GAB', 'GBR', 'GHA', 'GMB', 'GRC', 'GTM', 'HKG', 'HND', 'HTI', 'HUN', 
  'IDN', 'IND', 'IRL', 'IRQ', 'ISL', 'ISR', 'ITA', 'JAM', 'JOR', 'JPN', 'KEN', 
  'KHM', 'KOR', 'KWT', 'LAO', 'LBR', 'MAR', 'MDG', 'MDV', 'MEX', 'MLI', 'MLT', 
  'MMR', 'MNG', 'MRT', 'MUS', 'MWI', 'MYS', 'NER', 'NGA', 'NIC', 'NLD', 'NOR', 
  'NPL', 'NZL', 'PAK', 'PAN', 'PER', 'PHL', 'POL', 'PRT', 'PRY', 'RWA', 'SAU', 
  'SDN', 'SEN', 'SGP', 'SLE', 'SLV', 'SWE', 'SYR', 'TGO', 'THA', 'TTO', 'TUN', 
  'TUR', 'TZA', 'UGA', 'URY', 'USA', 'VEN', 'VNM', 'ZAF', 'ZMB')

# Read in data and preliminary cleaning----------------------------------------

data_reg <- data.table::fread(here("data/ECI-growth-data_final.csv")) %>%
  select(any_of(total_vars)) %>%
  mutate(
    # ccode = countrycode(ccode, 'iso3c', 'country.name'),
    Penn_GDP_PPP = Penn_GDP_PPP * 1000000000,
    Penn_GDP_PPP_log = log(Penn_GDP_PPP),
    log_pop = log(population),
    oil_exports_share_country = oil_exports_share_country*100, 
    coal_and_metal_exports_share_country = coal_and_metal_exports_share_country*100,
    HighIncome = ifelse(GNIpc>threshold_high_income, 1, 0),
    LowIncome = ifelse(GNIpc<=threshold_low_income, 1, 0),
    LowerMiddleIncome = ifelse(
      GNIpc>threshold_low_income & GNIpc<=threshold_lowermiddle_income, 1, 0),
    HigherMiddleIncome = ifelse(
      GNIpc>threshold_lowermiddle_income & GNIpc<=threshold_high_income, 1, 0)
  ) %>%
  filter(
    ccode %in% countries_considered
  ) %>%
  group_by(ccode) %>%
  mutate(
    (pop_growth = log_pop - dplyr::lag(log_pop))*100
  ) %>%
  ungroup() 

# Create 1985 - 2014 data------------------------------------------------------
data_reg_1985 <- data_reg %>%
  filter(Year==1985) %>%
  select(all_of(c(vars_used_avg, vars_used)))

data_reg_1985_2014 <- data_reg %>%
  filter(Year>=1985, Year<=2014) %>%
  select(all_of(c(vars_used, vars_used_avg, "period")))

regression_data_1_year <- data_reg %>%
  filter(Year>=1985, Year<=2014) %>%
  select(all_of(c(vars_used, vars_used_avg, "Year")))

data_reg_1985_2014_AVG <- data_reg_1985_2014 %>% 
  dplyr::group_by(ccode) %>% 
  dplyr::summarise(
    across(
      .cols = where(is.double), .fns = mean, na.rm=T, .names = "{.col}_Mean"), 
    .groups = "drop") %>%
  dplyr::select(all_of(
    c("ccode", "pop_growth_Mean", "hc_Mean", 
      "primary_exports_1_share_country_Mean",
      "oil_exports_share_country_Mean", 
      "coal_and_metal_exports_share_country_Mean",
      "GDP_pc_growth_Mean", "KOF_econ_Mean", "pop_growth_Mean", "hc_Mean",
      "inv_share_Mean", "domesticcredit_Mean")))

data_reg_predict_1985_2014 <- data_reg_1985 %>%
  select(all_of(
    c("ccode", "eci", "Penn_GDP_PPP_log", "AdvancedCountry", "HighIncome", 
      "LowIncome", "LowerMiddleIncome", "HigherMiddleIncome", "OPECdummy")
  )) %>%
  inner_join(., data_reg_1985_2014_AVG, by=c("ccode"))%>%
  dplyr::rename(
    avg_GDP_pc_PPP_growth=GDP_pc_growth_Mean,
    kof_econ=KOF_econ_Mean,
    GDP_pc_PPP_log=Penn_GDP_PPP_log,
    popgrowth=pop_growth_Mean,
    humancapital=hc_Mean,
    inv_share=inv_share_Mean,
    primaryexports=primary_exports_1_share_country_Mean,
    oilexports=oil_exports_share_country_Mean,
    coalandmetalexports=coal_and_metal_exports_share_country_Mean,
    domesticcredit=domesticcredit_Mean
  )

# Create 1990 - 2010 data------------------------------------------------------

data_reg_1990 <- data_reg %>%
  filter(Year==1990) %>%
  select(all_of(c(vars_used_avg, vars_used)))

data_reg_1990_2010 <- data_reg %>%
  filter(Year>=1990, Year<=2010) %>%
  select(all_of(c(vars_used_avg, vars_inst)))

data_reg_1990_2010_AVG <- data_reg_1990_2010 %>% 
  dplyr::group_by(ccode) %>% 
  dplyr::summarise(
    across(
      .cols = where(is.numeric), .fns = mean, na.rm=T, .names = "{.col}_Mean"), 
    .groups = "drop") %>%
  dplyr::select(all_of(
    c("ccode", "pop_growth_Mean", "hc_Mean",
      "primary_exports_1_share_country_Mean",
      "oil_exports_share_country_Mean", 
      "coal_and_metal_exports_share_country_Mean",
      "GDP_pc_growth_Mean", "KOF_econ_Mean", "pop_growth_Mean", "hc_Mean",
      "political_rel_Mean", "economic_rel_Mean", "legal_rel_Mean",
      "PropertyRights_Mean", "domesticcredit_Mean")))

data_reg_predict_1990_2010 <- data_reg_1990 %>%
  select(all_of(
    c("ccode", "eci", "Penn_GDP_PPP_log", "AdvancedCountry", "HighIncome", 
      "LowIncome", "LowerMiddleIncome", "HigherMiddleIncome", "OPECdummy", 
      "inv_share")
  )) %>%
  inner_join(
  ., data_reg_1990_2010_AVG, by=c("ccode"))  %>%
  dplyr::rename(
    avg_GDP_pc_PPP_growth=GDP_pc_growth_Mean,
    kof_econ=KOF_econ_Mean,
    GDP_pc_PPP_log=Penn_GDP_PPP_log,
    popgrowth=pop_growth_Mean,
    humancapital=hc_Mean,
    # inv_share=inv_share_Mean,
    primaryexports=primary_exports_1_share_country_Mean,
    oilexports=oil_exports_share_country_Mean,
    coalandmetalexports=coal_and_metal_exports_share_country_Mean,
    legalquality=legal_rel_Mean,
    politicalquality=political_rel_Mean,
    economicquality=economic_rel_Mean,
    propertyrights=PropertyRights_Mean,
    domesticcredit=domesticcredit_Mean
  ) %>%
  filter(
    !is.na(economicquality), !is.na(legalquality), !is.na(politicalquality)
  )

# Create 1970 - 1984 data------------------------------------------------------

data_reg_1970 <- data_reg %>%
  filter(Year==1970) %>%
  select(all_of(c(vars_used_avg, vars_used)))

data_reg_1970_1984 <- data_reg %>%
  filter(Year>=1970, Year<=1984) %>%
  select(all_of(c(vars_used_avg, vars_inst)))

data_reg_1970_1984_AVG <- data_reg_1970_1984 %>% 
  dplyr::group_by(ccode) %>% 
  dplyr::summarise(
    across(
      .cols = where(is.numeric), .fns = mean, na.rm=T, .names = "{.col}_Mean"), 
    .groups = "drop") %>%
  dplyr::select(all_of(
    c("ccode", "pop_growth_Mean", "hc_Mean", 
      "primary_exports_1_share_country_Mean",
      "oil_exports_share_country_Mean", 
      "coal_and_metal_exports_share_country_Mean",
      "GDP_pc_growth_Mean", "KOF_econ_Mean", "pop_growth_Mean", "hc_Mean",
      "domesticcredit_Mean")))

data_reg_predict_1970_1984 <- data_reg_1970 %>%
  select(all_of(
    c("ccode", "eci", "Penn_GDP_PPP_log", "AdvancedCountry", "HighIncome", 
      "LowIncome", "LowerMiddleIncome", "HigherMiddleIncome", "OPECdummy")
  )) %>%
  inner_join(
  ., data_reg_1970_1984_AVG, by=c("ccode"))  %>%
  dplyr::rename(
    avg_GDP_pc_PPP_growth=GDP_pc_growth_Mean,
    kof_econ=KOF_econ_Mean,
    GDP_pc_PPP_log=Penn_GDP_PPP_log,
    popgrowth=pop_growth_Mean,
    humancapital=hc_Mean,
    # inv_share=inv_share_Mean,
    primaryexports=primary_exports_1_share_country_Mean,
    oilexports=oil_exports_share_country_Mean,
    coalandmetalexports=coal_and_metal_exports_share_country_Mean,
    domesticcredit=domesticcredit_Mean
  )

# Create data 1985 - 2014 for developing countries-----------------------------

data_reg_predict_1985_2014_developing <- data_reg_predict_1985_2014 %>%
  filter(AdvancedCountry == 0)

# Create data with 5-year averages for panel estimation------------------------

regression_data_5_year <- data_reg_1985_2014 %>%
  group_by(ccode, period) %>%
  summarise(across(.cols = everything(), .fns = ~mean(.x, na.rm = T)),
            .groups="drop") %>%
  select(all_of(
    c("ccode", "period", "eci", "Penn_GDP_PPP_log", "GDP_pc_growth", 
      "KOF_econ", "AdvancedCountry", "HighIncome", "LowIncome", 
      "LowerMiddleIncome", "HigherMiddleIncome", "pop_growth", "hc", 
      "OPECdummy", "inv_share"))) %>%
  rename(
    GDP_pc_PPP_log=Penn_GDP_PPP_log, 
    kof_econ=KOF_econ,
    popgrowth=pop_growth,
    humancapital=hc
  )

# Save intermediate data-------------------------------------------------------
save(
  data_reg_predict_1985_2014,
  data_reg_predict_1990_2010,
  data_reg_predict_1970_1984,
  regression_data_1_year,
  regression_data_5_year,
  data_reg_predict_1985_2014_developing, 
  data_reg_1985_2014,
  file = here("data/intermediate_data.RData")
  )
