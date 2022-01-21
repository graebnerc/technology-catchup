suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(countrycode))
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(sandwich))
suppressPackageStartupMessages(library(stargazer))
suppressPackageStartupMessages(library(plm))
suppressPackageStartupMessages(library(margins))
suppressPackageStartupMessages(library(icaeDesign))
suppressPackageStartupMessages(library(sjPlot))
source(here("code/regressions_funs.R"))

redo_computation <- TRUE
vary_only_start <- TRUE # varies only start years and runs always until 2016
eci_color_mapping <- c("-2"="#1B9E77", "0"="#D95F02", "2"="#7570B3")
gdp_color_mapping <- c("7"="#1B9E77", "8.5"="#D95F02", "10"="#7570B3")

if (redo_computation){
  # 1.1. Data setup------------------------------
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
  
  #' Create data for cross-country growth regressions
  #' 
  #' @param init_data The raw data
  #' @param start_date The first year to be considered
  #' @param end_date The last year to be considered
  #' @return data.frame
  get_reg_data <- function(init_data, start_date, end_date){
    # ADD PARAMETERS
    
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
  
  
  min_range <- 1962:2010
  # min_range <- 1980:1981
  # min_range <- 2010:2012
  full_dat_list <- list()
  
  for (mi in min_range){
    if (vary_only_start){
      max_range <- 2014
    } else{
      max_range <- seq(mi+4, 2016)
    }
    for (ma in max_range) {
      print(paste(mi, "-", ma))
      
      reg_res <- tryCatch(expr = {
        
        current_data <- get_reg_data(
          init_data = base_data_reg, 
          start_date = mi, 
          end_date = ma)
        
        regr_form <- as.formula(
          paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + ", 
                 "eci*GDP_pc_PPP_log + popgrowth + humancapital + inv_share + OPECdummy*eci")
        )
        
        reg_man <- lm(formula = regr_form, data=current_data, na.action=na.exclude)
        
        # Make the plot with GDP on the x-axis:
        eci_xaxis_plot_vals <- plot_model(
          reg_man, type = "pred", 
          terms = c("eci", "GDP_pc_PPP_log [7, 8.5, 10]"), 
          ci.lvl=NA) + 
          scale_color_manual(
            values = gdp_color_mapping, 
            aesthetics = c("color", "fill"))
        
        eci_xaxis_plot_data <- ggplot_build(eci_xaxis_plot_vals)
        
        eci_xaxis_plot_vals_elements <- tibble(
          eci_xaxis_plot_data$data[[1]]) %>%
          select(all_of(c("colour", "y", "x"))) %>%
          mutate(
            col_val=ifelse(
              colour == gdp_color_mapping["7"], "7", ifelse(
                colour == gdp_color_mapping["8.5"], "8.5", ifelse(
                  colour == gdp_color_mapping["10"], "10", NA)
                )
              ),
            col_var = "GDP"
            ) %>%
          select(-all_of(c("colour")))
        
        # Make the plot with ECI on the x-axis:
        
        gdp_xaxis_plot_vals <- plot_model(
          reg_man, type = "pred", 
          terms = c("GDP_pc_PPP_log", "eci [-2, 0, 2]"),
          ci.lvl=NA) + 
          scale_color_manual(
            values = eci_color_mapping, 
            aesthetics = c("color", "fill"))
        
        gdp_xaxis_plot_data <- ggplot_build(gdp_xaxis_plot_vals)
        
        gdp_xaxis_plot_vals_elements <- tibble(
          gdp_xaxis_plot_data$data[[1]]) %>%
          select(all_of(c("colour", "y", "x"))) %>%
          mutate(
            col_val=ifelse(
              colour == eci_color_mapping["-2"], "-2", ifelse(
                colour == eci_color_mapping["0"], "0", ifelse(
                  colour == eci_color_mapping["2"], "2", NA)
              )
            ),
            col_var = "ECI"
          ) %>%
          select(-all_of(c("colour")))
        

        cplot_data_final <- rbind(
          eci_xaxis_plot_vals_elements, gdp_xaxis_plot_vals_elements) %>%
          mutate(period=paste(mi, "-", ma),
                 first_year = mi, last_year = ma)
      }, 
      error=function(cond) {
        message(paste("Error with estimation for the period:", mi, "-", ma))
        message("Here's the original error message:")
        message(cond)
        cplot_data_final <- NA
      })
      full_dat_list[[paste0(mi, "_", ma)]] <- reg_res
    }
  }
  red_dat_list <- full_dat_list[!is.na(full_dat_list)]
  full_plot_list <- rbindlist(red_dat_list)
  fwrite(full_plot_list, file = here("output/robustness/robustness_est_new.csv"))
}

# Visualization----------------------------------

plot_data <- fread(here("output/robustness/robustness_est_new.csv"), 
                   select = c(
                     "y"="double", "x"="double", "col_val"="factor", 
                     "col_var"="factor", "period"="factor", 
                     "first_year"="double", "last_year"="double"
                   ))

robustness_gdp <- plot_data %>%
  filter(first_year<2010, col_var=="ECI") %>%
  unite("period_col_val", period, col_val, remove = FALSE) %>%
  ggplot(., aes(x=x, y=y, color=col_val, group=period_col_val)) +
  geom_line(alpha=0.5, key_glyph=draw_key_rect)  + 
  labs(
    title = "Marginal effect of GDP in 1st year",
    x = "GDP per capita in first year (log)",
    y = "Predicted average growth in GDP pc"
  ) +
  scale_color_manual(
    values = eci_color_mapping, 
    aesthetics = c("color", "fill")) +
  guides(color=guide_legend(title="ECI")) +
  theme_icae() +
  theme(legend.title = element_text())
robustness_gdp

robustness_eci <- plot_data %>%
  filter(first_year<2010, col_var=="GDP") %>%
  unite("period_col_val", period, col_val, remove = FALSE) %>%
  ggplot(., aes(x=x, y=y, color=col_val, group=period_col_val)) +
  geom_line(alpha=0.5, key_glyph=draw_key_rect)  + 
  labs(
    title = "Marginal effect of ECI",
    x = "Economic Complexity Index",
    y = "Predicted average growth in GDP pc"
  ) +
  scale_color_manual(
    values = gdp_color_mapping, 
    aesthetics = c("color", "fill")) +
  guides(color=guide_legend(title="GDPpc in first year (log)")) +
  theme_icae() +
  theme(legend.title = element_text())
robustness_eci

# A single plot----------------------------------
robustness_full <- ggpubr::ggarrange(
  robustness_gdp, robustness_eci, ncol = 2, labels = c("A)", "B)")
)

robustness_full <- ggpubr::annotate_figure(
  robustness_full, 
  top = ggpubr::text_grob("Robustness of marginal effects for varying starting years", size = 14))

ggsave(plot = robustness_full, 
       filename = here("output/fig_4_robustness_new_v1.pdf"), 
       width = 9, height = 4)

# Separate plots----------------------------------
robustness_full2 <- ggpubr::ggarrange(
  robustness_gdp + facet_wrap(~col_val, nrow = 3) + theme(
    strip.background = element_blank(), strip.text.x = element_blank()), 
  robustness_eci + facet_wrap(~col_val, nrow = 3) + theme(
    strip.background = element_blank(), strip.text.x = element_blank()), 
  ncol = 2, labels = c("A)", "B)")
)

robustness_full2 <- ggpubr::annotate_figure(
  robustness_full2, 
  top = ggpubr::text_grob("Robustness of marginal effects for varying starting years", size = 14))

ggsave(plot = robustness_full2, 
       filename = here("output/fig_4_robustness_new_v2.pdf"), 
       width = 9, height = 4)

