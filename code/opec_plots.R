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
source(here("code/regressions_funs.R"))

redo_computation <- TRUE
variable_of_interest <- "eci"#  "eci" # "GDP_pc_PPP_log" 
vary_only_start <- TRUE # varies only start years and runs always until 2016
if (vary_only_start){
  analysis_result_file <- here(
    paste0("output/opec/estimations_", variable_of_interest, "_red.csv"))
  analysis_result_file_mpdata <- here(
    paste0("output/opec/mpdata_", variable_of_interest, "_red.csv"))
  pdf_file_name <- here(paste0(
    "output/opec/opec_shit_red.pdf"))
  txt_file <- here(paste0("output/opec/period_info_red.txt"))
} else {
  analysis_result_file <- here(
    paste0("output/opec/estimations_", variable_of_interest, ".csv"))
  analysis_result_file_mpdata <- here(
    paste0("output/opec/mpdata_", variable_of_interest, ".csv"))
  pdf_file_name <- here(paste0(
    "output/opec/opec_shit.pdf"))
  txt_file <- here(paste0("output/opec/period_info.txt"))
}

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
  
  
  min_range <- 1962:2012
  # min_range <- 1980:1981
  # min_range <- 2010:2012
  full_dat_list <- list()
  
  for (mi in min_range){
    if (vary_only_start){
      max_range <- 2016
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
        
        opec_zero <- cplot(
          object =reg_man, 
          dx = "eci", 
          x = "GDP_pc_PPP_log", 
          data = filter(current_data, OPECdummy==0),  
          what = "effect", 
          draw = FALSE,
          xvals = seq(6, 12, 0.1)
        )
        opec_one <- cplot(
          object =reg_man, 
          dx = "eci", 
          x = "GDP_pc_PPP_log", 
          data = filter(current_data, OPECdummy==1),  
          what = "effect", 
          draw = FALSE,
          xvals = seq(6, 12, 0.1)
        )
        
        no_opec_gg <- tibble(opec_zero) %>%
          mutate(group="No OPEC") 
        opec_gg <- tibble(opec_one) %>%
          mutate(group="OPEC") 
        all_opec_gg <- rbind(no_opec_gg, opec_gg) %>%
          mutate(period=paste(mi, "-", ma),
                 first_year = mi, last_year = ma)
        all_opec_gg
      }, 
      error=function(cond) {
        message(paste("Error with estimation for the period:", mi, "-", ma))
        message("Here's the original error message:")
        message(cond)
        all_opec_gg <- NA
        all_opec_gg
      })
      full_dat_list[[paste0(mi, "_", ma)]] <- reg_res
    }
  }
  red_dat_list <- full_dat_list[!is.na(full_dat_list)]
  full_plot_list <- rbindlist(red_dat_list)
  fwrite(full_plot_list, file = here("output/opec/opec_est.csv"))
}

# Visualization----------------------------------
make_ggplot <- function(data, per_cons){
  ggplot(data = data, 
         mapping = aes(x=xvals, y=yvals, group=period, color=slope)) +
    geom_line(alpha=0.25) +
    labs(title = per_cons, x="Income level", y="Marg effect of ECI") +
    #coord_cartesian(ylim = c(-2.5, 5)) +
    scale_color_manual(values = c("negative"="#e60000", "positive"="#004d00")) +
    theme_icae() +
    theme(
      legend.title = element_blank(), 
      legend.position = "none", 
      plot.title = element_text(size=10),
      panel.border = element_blank(),
      axis.line = element_line())
}

opec_plot_data <- fread(here("output/opec/opec_est.csv"))

opec_plot_data %>%
  ggplot(., aes(x=xvals, y=yvals, group=period)) +
  geom_line(color="grey", alpha=0.75) + 
  facet_wrap(~group) + theme_icae()

effect_slope <- opec_plot_data %>%
  select(all_of(c("xvals", "yvals", "period", "group"))) %>%
  filter(
    xvals %in% c(6.0, 12.0)
  ) %>%
  pivot_wider(id_cols = c("period", "group"), names_from = "xvals", values_from = "yvals") %>%
  mutate(
    slope = ifelse(`12` > `6`, "positive", 
                   ifelse(`12` < `6`, "negative", "horizontal"))
  ) %>%
  select(all_of(c("period", "slope", "group")))

if (vary_only_start){
  mpdata_plot <- left_join(opec_plot_data, effect_slope, by = c("period", "group")) %>%
    mutate(
      period2 = ifelse(
        first_year %in% 1962:1973, "Start 1962-1973", ifelse(
          first_year %in% 1974:1984, "Start 1974-1984", ifelse(
                first_year %in% 1985:1996, "Start 1985-1996", ifelse(
                  first_year>1996, "Start 1997 and later", "Remainder"))))
    )
  # Make actual plots
  p_cons <- "Start 1962-1973"
  p1_plot <- mpdata_plot %>%
    filter(period2 == p_cons) %>%
    make_ggplot(p_cons) +
    facet_wrap(~group) +
    ggrepel::geom_text_repel(
      data = filter(mpdata_plot, xvals==6.0, period2 == p_cons), 
      mapping = aes(color=slope, label=first_year)) +
    scale_x_continuous(expand = expansion(add = c(2, 0)))
  p1_plot
  
  p_cons <- "Start 1974-1984"
  p2_plot <- mpdata_plot %>%
    filter(period2 == p_cons) %>%
    make_ggplot(p_cons) +
    facet_wrap(~group) +
    ggrepel::geom_text_repel(
      data = filter(mpdata_plot, xvals==6.0, period2 == p_cons), 
      mapping = aes(color=slope, label=first_year)) +
    scale_x_continuous(expand = expansion(add = c(2, 0)))
  p2_plot
  
  p_cons <- "Start 1985-1996"
  p3_plot <- mpdata_plot %>%
    filter(period2 == p_cons) %>%
    make_ggplot(p_cons) +
    facet_wrap(~group) +
    ggrepel::geom_text_repel(
      data = filter(mpdata_plot, xvals==6.0, period2 == p_cons), 
      mapping = aes(color=slope, label=first_year)) +
    scale_x_continuous(expand = expansion(add = c(2, 0)))
  p3_plot
  
  p_cons <- "Start 1997 and later"
  p4_plot <- mpdata_plot %>%
    filter(period2 == p_cons) %>%
    make_ggplot(p_cons) +
    facet_wrap(~group) +
    ggrepel::geom_text_repel(
      data = filter(mpdata_plot, xvals==6.0, period2 == p_cons), 
      mapping = aes(color=slope, label=first_year)) +
    scale_x_continuous(expand = expansion(add = c(2, 0)))
  p4_plot
  
  p1_6_plot <- ggpubr::ggarrange(
    p1_plot, p2_plot, p3_plot,
    p4_plot, 
    ncol = 2, nrow = 2
  )
  
  ggsave(plot = p1_6_plot, 
         filename = pdf_file_name, 
         width = 10, height = 9)
  
} else {
  mpdata_plot <- left_join(opec_plot_data, effect_slope, by = c("period", "group")) %>%
    mutate(
      period2 = ifelse(
        first_year %in% 1962:1973, "From 1962-1973 until later", ifelse(
          first_year %in% 1974:1984 & last_year<1991, "From 1974-1984 until 1990", ifelse(
            first_year %in% 1974:1984 & last_year>=1991, "From 1974-1984 until after 1990", ifelse(
              first_year %in% 1997:1999, "From 1997-1999 until later", ifelse(
                first_year %in% 1985:1996, "From 1985-1996 until later", ifelse(
                  first_year>1999, "From 1999 until later", "Remainder"))))))
    )
  
  # Make actual plots
  p_cons <- "From 1962-1973 until later"
  p1_plot <- mpdata_plot %>%
    filter(period2 == p_cons) %>%
    make_ggplot(p_cons) +
    facet_wrap(~group)
  p1_plot
  
  p_cons <- "From 1974-1984 until 1990"
  p2_plot <- mpdata_plot %>%
    filter(period2 == p_cons) %>%
    make_ggplot(p_cons) +
    facet_wrap(~group)
  p2_plot
  
  p_cons <- "From 1974-1984 until after 1990"
  p3_plot <- mpdata_plot %>%
    filter(period2 == p_cons) %>%
    make_ggplot(p_cons) +
    facet_wrap(~group)
  p3_plot
  
  p_cons <- "From 1985-1996 until later"
  p4_plot <- mpdata_plot %>%
    filter(period2 == p_cons) %>%
    make_ggplot(p_cons) +
    facet_wrap(~group)
  p4_plot
  
  p_cons <- "From 1997-1999 until later"
  p5_plot <- mpdata_plot %>%
    filter(period2 == p_cons) %>%
    make_ggplot(p_cons) +
    facet_wrap(~group)
  p5_plot
  
  p_cons <- "From 1999 until later"
  p6_plot <- mpdata_plot %>%
    filter(period2 == p_cons) %>%
    make_ggplot(p_cons) +
    facet_wrap(~group)
  p6_plot
  
  p1_6_plot <- ggpubr::ggarrange(
    p1_plot, p2_plot, p3_plot,
    p4_plot, p5_plot, p6_plot,
    ncol = 3, nrow = 2
  )
  
  ggsave(plot = p1_6_plot, 
         filename = pdf_file_name, 
         width = 10, height = 6)
}


#Infos zu positiven und negatien Slopes----------

positiv_opec <- effect_slope %>%
  filter(group=="OPEC", slope=="positive") %>%
  pull(period)

negative_opec <- effect_slope %>%
  filter(group=="OPEC", slope=="negative") %>%
  pull(period)

positiv_no_opec <- effect_slope %>%
  filter(group=="No OPEC", slope=="positive") %>%
  pull(period)

negative_no_opec <- effect_slope %>%
  filter(group=="No OPEC", slope=="negative") %>%
  pull(period)

write("Periods for OPEC countries with positive slopes", file = txt_file, append = FALSE)
write("===============================================\n", file = txt_file, append = TRUE)
write(paste(positiv_opec, collapse = "\n"), file = txt_file, append = TRUE)

write("\nPeriods for Non-OPEC countries with positive slopes", file = txt_file, append = TRUE)
write("===================================================\n", file = txt_file, append = TRUE)
write(paste(positiv_no_opec, collapse = "\n"), file = txt_file, append = TRUE)

write("\nPeriods for OPEC countries with negative slopes", file = txt_file, append = TRUE)
write("===============================================\n", file = txt_file, append = TRUE)
write(paste(negative_opec, collapse = "\n"), file = txt_file, append = TRUE)

write("\nPeriods for Non-OPEC countries with negative slopes", file = txt_file, append = TRUE)
write("===================================================\n", file = txt_file, append = TRUE)
write(paste(negative_no_opec, collapse = "\n"), file = txt_file, append = TRUE)
