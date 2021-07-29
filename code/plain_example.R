suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(countrycode))
suppressPackageStartupMessages(library(icaeDesign))

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

# ADJUST THESE PARAMETERS
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
  "coal_and_metal_exports_share_country")

total_vars <- c(
  "Year", "Penn_GDP_PPP", "population", "GNIpc", "PropertyRights", "period",
  "propertyrights", "politicalquality", "economicquality", "legalquality",
  vars_used_avg, vars_used, vars_inst)

threshold_high_income <- 12055
threshold_low_income <- 995.99
threshold_lowermiddle_income <- 3895

base_data_reg <- data.table::fread(here("data/ECI-growth-data_final.csv")) %>%
  select(any_of(total_vars)) %>%
  mutate(
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
  ungroup() %>%
  filter(!is.na(eci), !is.na(Penn_GDP_PPP))

# Beispiel---------------------------------------
first_ob <- min(base_data_reg$Year)
expl_countries <- c("DEU", "AUT", "BEL", 'UGA', 'URY', 'USA')

plain_plot <- base_data_reg %>%
  filter(ccode %in% expl_countries) %>%
  arrange(Year, .by_group = TRUE) %>%
  ggplot(data = ., aes(x=Penn_GDP_PPP, y=eci, color=ccode)) +
  geom_path(arrow = arrow(angle = 12, ends = "last", type = "closed")) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "ECI and GDP 1962 - 2016") +
  geom_label(data = filter(
    base_data_reg, Year==first_ob, ccode %in% expl_countries), 
    mapping = aes(label=ccode), show.legend = FALSE) +
  theme_icae()

ggsave(plot = plain_plot, 
       filename = here("output/plan_plot_expl.pdf"), 
       width = 8, height = 8)


