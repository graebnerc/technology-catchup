suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(countrycode))
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(sandwich))
suppressPackageStartupMessages(library(stargazer))
suppressPackageStartupMessages(library(plm))
source(here("code/regressions_funs.R"))

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

#' Create data for cross-country growth regressions
#' 
#' @param init_data The raw data
#' @param start_date The first year to be considered
#' @param end_date The last year to be considered
#' @return data.frame
get_reg_data <- function(init_data, start_date, end_date){
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
  
  # ACTUAL START
  data_reg_firstyear <- init_data %>%
    filter(Year==start_date) %>%
    select(all_of(c(vars_used_avg, vars_used)))
  
  data_reg_first_last <- init_data %>%
    filter(Year>=start_date, Year<=end_date) %>%
    select(all_of(c(vars_used, vars_used_avg, "period")))
  
  regression_data_1_year <- init_data %>%
    filter(Year>=start_date, Year<=end_date) %>%
    select(all_of(c(vars_used, vars_used_avg, "Year")))
  
  data_reg_first_last_AVG <- data_reg_first_last %>% 
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
        "inv_share_Mean")))
  
  data_reg_predict <- data_reg_firstyear %>%
    select(all_of(
      c("ccode", "eci", "Penn_GDP_PPP_log", "AdvancedCountry", "HighIncome", 
        "LowIncome", "LowerMiddleIncome", "HigherMiddleIncome", "OPECdummy")
    )) %>%
    inner_join(., data_reg_first_last_AVG, by=c("ccode"))%>%
    dplyr::rename(
      avg_GDP_pc_PPP_growth=GDP_pc_growth_Mean,
      kof_econ=KOF_econ_Mean,
      GDP_pc_PPP_log=Penn_GDP_PPP_log,
      popgrowth=pop_growth_Mean,
      humancapital=hc_Mean,
      inv_share=inv_share_Mean,
      primaryexports=primary_exports_1_share_country_Mean,
      oilexports=oil_exports_share_country_Mean,
      coalandmetalexports=coal_and_metal_exports_share_country_Mean
    )
}

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
  ungroup() 

# -----------------------------------------------------------------------------

start_year <- 1985
end_year <- 2014
variable_of_interest <- "eci"


min_range <- 1962:2012

est_list <- list()
for (mi in min_range){
  for (ma in ((mi+4):2016)) {
    print(paste(mi, "-", ma))
    
    full_reg_results <- tryCatch(expr = {
      
      current_data <- get_reg_data(
        init_data = base_data_reg, 
        start_date = mi, 
        end_date = ma)
      
      full_reg <- make_reg(
        as.formula(
          paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + ", 
                 "eci*GDP_pc_PPP_log + popgrowth + humancapital + inv_share")
        ), 
        reg_data=current_data)
      
      full_reg_results <- list(
        variable_of_interest = "eci",
        estimate_ =  full_reg[["reg"]][["coefficients"]][[variable_of_interest]],
        pvalue_ = full_reg[["pvals"]][[1]][[variable_of_interest]]
      )
    }, 
    error=function(cond) {
      message(paste("Error with estimation for the period:", mi, "-", ma))
      message("Here's the original error message:")
      message(cond)
      
      full_reg_results <- list(
        variable_of_interest = variable_of_interest,
        estimate_ =  NA,
        pvalue_ = NA
      )
      return(full_reg_results)
    }
    )
    
    res_table <- tibble::tibble(
      start_year = mi, 
      end_year = ma,
      variable = full_reg_results[["variable_of_interest"]],
      estimate = full_reg_results[["estimate_"]],
      pvalue = full_reg_results[["pvalue_"]]
    )
    
    est_list[[paste0(mi, "_", ma)]] <- res_table
  }
}

final_eci <- data.table::rbindlist(est_list)
fwrite(final_eci, file = here("output/eci_estimations.csv"))

# Make the heatmap-------------------------------------------------------------

final_eci_map <- final_eci %>%
  dplyr::mutate(
    est_p = ifelse(pvalue<0.1, estimate, 0)
  ) %>%
  ggplot(data = ., aes(y = start_year, x = end_year)) + 
  geom_tile(aes(fill = est_p)) + 
  scale_fill_viridis_c(option = "B") +
  theme_bw() +
  labs(
    title = "Estimated values for ECI", 
    x = "Start year", y = "End year", 
    caption = "Grey cells indicate uncomputable, black cells insignificant estimates."
    ) +
  scale_x_continuous(expand = expansion()) + 
  scale_y_continuous(expand = expansion()) + 
  theme(
    panel.grid = element_blank(), 
    legend.position = "bottom", 
    legend.title = element_blank()
    )

ggsave(plot = final_eci_map, 
       filename = here("output/eci_heatmap.pdf"), 
       height = 6, width = 5)
