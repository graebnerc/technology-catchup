suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(countrycode))
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(sandwich))
suppressPackageStartupMessages(library(stargazer))
suppressPackageStartupMessages(library(plm))
suppressPackageStartupMessages(library(margins))
source(here("code/regressions_funs.R"))

compute_estimates <- FALSE # If TRUE step 1 below gets computed
# This takes some time, so only reasonable if underlying regressions changed
make_marg_plot_data <- TRUE # If TRUE, computes marginal effect plot data
# Takes even longer, so use carefully!

variable_of_interest <- "eci"#  "eci" # "GDP_pc_PPP_log" 
analysis_result_file <- here(
  paste0("output/heatmaps/estimations_", variable_of_interest, ".csv"))
analysis_result_file_mpdata <- here(
  paste0("output/heatmaps/mpdata_", variable_of_interest, ".csv"))

# 1. Compute the estimates (optional)------------

if (compute_estimates){
  
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
  
  # 1.2. The actual analysis loop----------------
  start_year <- 1985
  end_year <- 2014
  
  min_range <- 1962:2012
  # min_range <- 1980:1981

  est_list <- list()
  if (make_marg_plot_data){
    marg_plots <- list()
  }
  for (mi in min_range){
    for (ma in ((mi+4):2016)) {
      print(paste(mi, "-", ma))
      
      marg_plot_full_reg_results <- tryCatch(expr = {
        
        current_data <- get_reg_data(
          init_data = base_data_reg, 
          start_date = mi, 
          end_date = ma)
        
        regr_form <- as.formula(
          paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + ", 
                 "eci*GDP_pc_PPP_log + popgrowth + humancapital + inv_share")
        )
        
        full_reg <- make_reg(reg_formula = regr_form, 
          reg_data=current_data)
        
        marg_sum <- summary(full_reg[["marg"]])
        mem_marg_sum <- summary(full_reg[["mem_marg"]])
        
        
        marg_plot_full_reg_results[["full_reg_results"]] <- list(
          var_of_interest = variable_of_interest,
          estimate_ =  full_reg[["reg"]][["coefficients"]][[variable_of_interest]],
          pvalue_ = full_reg[["pvals"]][[1]][[variable_of_interest]],
          ame_est = unname(filter(tibble(marg_sum),
                                  factor==variable_of_interest)[["AME"]]),
          ame_pval = unname(filter(tibble(marg_sum),
                                   factor==variable_of_interest)[["p"]]),
          mem_est = unname(filter(tibble(mem_marg_sum),
                                  factor==variable_of_interest)[["AME"]]),
          mem_pval = unname(filter(tibble(mem_marg_sum),
                                   factor==variable_of_interest)[["p"]])
        )
        if (make_marg_plot_data){
          marg_plot_full_reg_results[["current_marg_plot"]] <- make_reg(
            reg_formula = regr_form, reg_data=current_data, get_marg_plot=TRUE, 
            marg_plot_dx="eci", marg_plot_x="GDP_pc_PPP_log",
            marg_plot_xvals=seq(6, 11, 0.1), first_year=mi, last_year=ma)
        }
        
        marg_plot_full_reg_results
      }, 
      error=function(cond) {
        message(paste("Error with estimation for the period:", mi, "-", ma))
        message("Here's the original error message:")
        message(cond)
        marg_plot_full_reg_results <- list()
        marg_plot_full_reg_results[["full_reg_results"]] <- list(
          var_of_interest = variable_of_interest,
          estimate_ =  NA,
          pvalue_ = NA,
          ame_est = NA,
          ame_pval= NA,
          mem_est = NA,
          mem_pval= NA
        )
        if (make_marg_plot_data){
          marg_plot_full_reg_results[["current_marg_plot"]] <- NA
        }
        return(marg_plot_full_reg_results)
      }
      )
      full_reg_results <- marg_plot_full_reg_results[["full_reg_results"]]
      
      res_table <- tibble::tibble(
        start_year = mi, 
        end_year = ma,
        variable = full_reg_results[["var_of_interest"]],
        estimate = full_reg_results[["estimate_"]],
        pvalue = full_reg_results[["pvalue_"]],
        ame_estimate = full_reg_results[["ame_est"]],
        ame_pvalue = full_reg_results[["ame_pval"]],
        mem_estimate = full_reg_results[["mem_est"]],
        mem_pvalue = full_reg_results[["mem_pval"]]
      )
      
      est_list[[paste0(mi, "_", ma)]] <- res_table
      if (make_marg_plot_data){
        marg_plots[[paste0(mi, "_", ma)]] <- marg_plot_full_reg_results[["current_marg_plot"]]
      }
      
    }
  }
  
  final_eci <- data.table::rbindlist(est_list)
  fwrite(final_eci, file = analysis_result_file)
  if (make_marg_plot_data){
    marg_plots_nonNA <- marg_plots[!is.na(marg_plots)]
    final_mpdata <- dplyr::bind_rows(marg_plots_nonNA)
    fwrite(final_mpdata, file = analysis_result_file_mpdata)
  }
}

# 2. Creation of the heatmap---------------------

final_eci <- fread(analysis_result_file, select = c(
  "start_year"="double", "end_year"="double", 
  "variable"="character",
  "estimate"="double", "pvalue"="double",
  "ame_estimate"="double", "ame_pvalue"="double",
  "mem_estimate"="double", "mem_pvalue"="double"
  ))

for (marg_effect in c("AME", "MEM", "Beta")){
  if (marg_effect=="AME"){
    marg_est <- "ame_estimate"
    marg_pval <- "ame_pvalue"
  } else if (marg_effect=="MEM"){
    marg_est <- "mem_estimate"
    marg_pval <- "mem_pvalue"
  } else if (marg_effect=="Beta"){
    marg_est <- "estimate"
    marg_pval <- "pvalue"
  } else {
    stop("WRONG MARG EFFECT SPECIFIED!!!")
  }
  col_max <- max(c(
    abs(min(final_eci[[marg_est]], na.rm = TRUE)),
    abs(max(final_eci[[marg_est]], na.rm = TRUE))
  ))
  col_range <- c(-col_max, col_max)
  
  # Version 1: insignificant estimates set to zero
  final_eci_map_v1 <- final_eci %>%
    dplyr::mutate(
      est_p = ifelse(!!as.name(marg_pval)<0.1, 
                     !!as.name(marg_est), 0)
    ) %>%
    ggplot(data = ., aes(y = start_year, x = end_year)) + 
    geom_tile(aes(fill = est_p)) + 
    scale_fill_gradient2(
      low = "#660066", high = "#006600", mid = "grey", 
      midpoint = 0, limits = col_range) +
    theme_bw() +
    labs(
      title = paste0("Estimated ", marg_effect, " for ", variable_of_interest), 
      y = "Start year", x = "End year", 
      caption = "Grey cells indicate uncomputable, black cells insignificant estimates."
    ) +
    scale_x_continuous(expand = expansion()) + 
    scale_y_continuous(expand = expansion()) + 
    theme(
      panel.grid = element_blank(), 
      legend.position = "bottom", 
      legend.title = element_blank()
    )
  
  ggsave(plot = final_eci_map_v1, 
         filename = here(
           paste0("output/heatmaps/", variable_of_interest, 
                  "_heatmap_v1_", marg_effect, ".pdf")), 
         height = 6, width = 5)
  
  # Version 2: insignificant results only marked
  
  final_eci_map_v2 <- final_eci %>%
    dplyr::mutate(
      significant = ifelse(!!as.name(marg_pval)<0.1, 1, 0)
    ) %>%
    ggplot(data = ., aes(y = start_year, x = end_year)) + 
    geom_tile(aes_string(fill = marg_est)) + 
    scale_fill_gradient2(
      low = "#660066", high = "#006600", mid = "grey", midpoint = 0, limits = col_range) +
    theme_bw() +
    labs(
      title = paste0("Estimated ", marg_effect," for ", variable_of_interest), 
      y = "Start year", x = "End year", 
      caption = "Grey cells indicate uncomputable, marked cells insignificant estimates."
    ) +
    scale_x_continuous(expand = expansion()) + 
    scale_y_continuous(expand = expansion()) + 
    theme(
      panel.grid = element_blank(), 
      legend.position = "bottom", 
      legend.title = element_blank()
    )
  
  final_eci_map_v2 <- final_eci_map_v2 + 
    geom_point(data = dplyr::filter(
      final_eci, !!as.name(marg_pval)>=0.1),
      shape=4, show.legend = FALSE, 
      color="black", alpha=0.45)
  
  ggsave(plot = final_eci_map_v2, 
         filename = here(
           paste0("output/heatmaps/", variable_of_interest, 
                  "_heatmap_v2_", marg_effect, ".pdf")), 
         height = 6, width = 5)
}

# 3. Creation of overall marginal effects plots------------
make_ggplot <- function(data, per_cons){
  ggplot(data = data, 
         mapping = aes(x=xvals, y=yvals, group=years, color=slope)) +
    geom_line(alpha=0.25) +
    labs(title = per_cons, x="Income level", y="Marg effect of ECI") +
    coord_cartesian(ylim = c(-2.5, 5)) +
    scale_color_manual(values = c("negative"="#e60000", "positive"="#004d00")) +
    theme_bw() +
    theme(
      legend.title = element_blank(), 
      legend.position = "none", 
      plot.title = element_text(size=10),
      panel.border = element_blank(),
      axis.line = element_line())
}

mpdata <- fread(file = analysis_result_file_mpdata, 
                colClasses = c(rep("double", 4), rep("character", 2)))

effect_slope <- mpdata %>%
  select(all_of(c("xvals", "yvals", "years"))) %>%
  filter(
    xvals %in% c(6.0, 11.0)
  ) %>%
  pivot_wider(id_cols = "years", names_from = "xvals", values_from = "yvals") %>%
  mutate(
    slope = ifelse(`11` > `6`, "positive", 
                   ifelse(`11` < `6`, "negative", "horizontal"))
  ) %>%
  select(all_of(c("years", "slope")))

positive_slope_years <- effect_slope %>%
  filter(slope=="positive") %>%
  pull("years")

negative_slope_years <- effect_slope %>%
  filter(slope=="negative") %>%
  pull("years")

horizontal_slope_years <- effect_slope %>%
  filter(slope=="horizontal") %>%
  pull("years")

mpdata_plot <- left_join(mpdata, effect_slope, by = "years") %>%
  mutate(
    first_year = as.double(substr(years, 1, 4)),
    last_year = as.double(substr(years, 6, 9)),
    period = ifelse(
      first_year %in% 1962:1973, "From 1962-1973 until later", ifelse(
        first_year %in% 1974:1984 & last_year<1991, "From 1974-1984 until 1990", ifelse(
          first_year %in% 1974:1984 & last_year>=1991, "From 1974-1984 until after 1990", ifelse(
            first_year %in% 1997:1999, "From 1997-1999 until later", ifelse(
              first_year %in% 1985:1996, "From 1985-1996 until later", ifelse(
                first_year>1999, "From 1999 until later", "Remainder"))))))
    )
p_cons <- "From 1962-1973 until later"
p1_plot <- mpdata_plot %>%
  filter(period == p_cons) %>%
  make_ggplot(p_cons)
p1_plot

p_cons <- "From 1974-1984 until 1990"
p2_plot <- mpdata_plot %>%
  filter(period == p_cons) %>%
  make_ggplot(p_cons)
p2_plot

p_cons <- "From 1974-1984 until after 1990"
p3_plot <- mpdata_plot %>%
  filter(period == p_cons) %>%
  make_ggplot(p_cons)
p3_plot

p_cons <- "From 1985-1996 until later"
p4_plot <- mpdata_plot %>%
  filter(period == p_cons) %>%
  make_ggplot(p_cons)
p4_plot

p_cons <- "From 1997-1999 until later"
p5_plot <- mpdata_plot %>%
  filter(period == p_cons) %>%
  make_ggplot(p_cons)
p5_plot

p_cons <- "From 1999 until later"
p6_plot <- mpdata_plot %>%
  filter(period == p_cons) %>%
  make_ggplot(p_cons) 
p6_plot

p1_6_plot <- ggpubr::ggarrange(
  p1_plot, p2_plot, p3_plot,
  p4_plot, p5_plot, p6_plot,
  ncol = 3, nrow = 2
)

ggsave(plot = p1_6_plot, 
       filename = here(paste0(
         "output/heatmaps/marg_effects_", variable_of_interest, ".pdf")), 
       width = 8, height = 6)
