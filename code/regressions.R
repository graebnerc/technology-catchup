library(tidyverse)
library(data.table)
library(margins)
library(sjPlot)
library(here)
library(countrycode)
library(lmtest)
library(sandwich)
library(stargazer)
library(plm)
library(latex2exp)

#' Conduct regressions with robust standard errors
#' 
#' Conducts a regression and returns heteroskedasticity-robust standard errors, 
#'  heteroskedasticity-robust t-values that also account for serial 
#'  (cross-sectional) correlation, and heteroskedasticity-robust p-vals.
#'  All regressions are conducted via `lm()` with the option 
#'  `na.action=na.exclude`.
#' 
#' @param reg_formula The regression fomula (as fomula object)
#' @param reg_data The data used for the regression
#' @return A list with four entries: `reg`, `ses`, `tvals`, and `pvals`
make_reg <- function(reg_formula, reg_data){
  
  reg_object <- lm(formula = reg_formula, data=reg_data, na.action=na.exclude)
  
  ses <- list(
    coeftest(reg_object, vcov.=function(x) vcovHC(x))[,2]) 
  tvals <- list(
    coeftest(reg_object, vcov.=function(x) vcovHC(x))[,3]) 
  pvals <- list(
    coeftest(reg_object, vcov.=function(x) vcovHC(x))[,4]) 
  final_list <- list(
    "reg" = reg_object,
    "ses" = ses,
    "tvals" = tvals,
    "pvals" = pvals
  )
}

#' Conduct panel regressions with robust standard errors
#' 
#' Conducts a plm regression and returns heteroskedasticity-robust standard 
#'  errors, heteroskedasticity-robust t-values that also account for serial 
#'  (cross-sectional) correlation, and heteroskedasticity-robust p-vals.
#'  All regressions are conducted via `plm()` with the option 
#'  `na.action=na.exclude`.
#' 
#' @param reg_formula The regression fomula (as fomula object)
#' @param reg_data The data used for the regression
#' @param panel_model The model for the estimation ("pooling" or "within")
#' @param panel_effect The kinf of effect used in within-models
#' @return A list with four entries: `reg`, `ses`, `tvals`, and `pvals`
make_panel_reg <- function(reg_formula, reg_data, panel_model, panel_effect=NULL){
  if (panel_model == "pooling"){
    reg_object <- plm(formula = reg_formula, data=reg_data, 
                      index=c("ccode", "period"), model=panel_model, 
                      na.action=na.exclude)
  }

  else if (panel_model == "within"){
    reg_object <- plm(formula = reg_formula, data=reg_data, 
                      index=c("ccode", "period"), model=panel_model, 
                      effect = "individual", na.action=na.exclude)
  } else{
    stop("No correct panel model specified!")
  } 
  
  ses <- list(
    coeftest(reg_object, vcov.=function(x) vcovHC(x, type="sss"))[,2]) 
  tvals <- list(
    coeftest(reg_object, vcov.=function(x) vcovHC(x, type="sss"))[,3]) 
  pvals <- list(
    coeftest(reg_object, vcov.=function(x) vcovHC(x, type="sss"))[,4]) 
  final_list <- list(
    "reg" = reg_object,
    "ses" = ses,
    "tvals" = tvals,
    "pvals" = pvals
  )
}
# Data used--------------------------------------------------------------------

load(file = here("data/intermediate_data.RData"))
#load(file = here("data/intermediate_data_old.Rdata"))
# data_reg_predict_1985_2014
# data_reg_predict_1990_2010
# data_reg_predict_1970_1984
# regression_data_5_year
# data_reg_predict_1985_2014_developing

# Main regressions-------------------------------------------------------------

reg_predict_1 <- make_reg(
  as.formula(paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log")), 
  reg_data=data_reg_predict_1985_2014)

reg_predict_2 <- make_reg(
  as.formula(paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + eci")), 
  reg_data=data_reg_predict_1985_2014)

reg_predict_3 <- make_reg(
  as.formula(paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + eci + kof_econ")), 
  reg_data=data_reg_predict_1985_2014)

reg_predict_4 <- make_reg(
  as.formula(
    paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + eci*GDP_pc_PPP_log")), 
  reg_data=data_reg_predict_1985_2014)

reg_predict_4_kof <- make_reg(
  as.formula(
    paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + ", 
           "eci*GDP_pc_PPP_log")), reg_data=data_reg_predict_1985_2014)

reg_predict_4_popgrowth <- make_reg(
  as.formula(
    paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + popgrowth + ", 
           "eci*GDP_pc_PPP_log")), reg_data=data_reg_predict_1985_2014)

reg_predict_4_humancapital <- make_reg(
  as.formula(
    paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + humancapital + ",
           "eci*GDP_pc_PPP_log")), reg_data=data_reg_predict_1985_2014)

reg_predict_6 <- make_reg(
  as.formula(
    paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + eci*kof_econ")), 
  reg_data=data_reg_predict_1985_2014)

reg_predict_7 <- make_reg(
  as.formula(
    paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + ",
           "eci*GDP_pc_PPP_log + popgrowth + humancapital")), 
  reg_data=data_reg_predict_1985_2014)

# data_reg_predict_1985_2014$predicted <- predict(reg_predict_7)   # Save the predicted values
# data_reg_predict_1985_2014$residuals <- residuals(reg_predict_7) # Save the residual values

# Regressions for robustness checks--------------------------------------------

reg_predict_7 <- make_reg(
  as.formula(
   paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + ", 
          "eci*GDP_pc_PPP_log + popgrowth + humancapital")
   ), 
  reg_data=data_reg_predict_1985_2014)

reg_predict_7_oilexports <- make_reg(
  as.formula(
    paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + ",
    "eci*GDP_pc_PPP_log + oilexports + popgrowth + humancapital")
    ), 
  reg_data=data_reg_predict_1985_2014)

reg_predict_7_coalandmetalexports <- make_reg(
  as.formula(
    paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + ",
    "eci*GDP_pc_PPP_log + coalandmetalexports + popgrowth + humancapital")
    ), 
  reg_data=data_reg_predict_1985_2014)

reg_predict_7_legalquality <- make_reg(
  as.formula(
    paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + ",
    "eci*GDP_pc_PPP_log + legalquality + popgrowth + humancapital")
    ), 
  reg_data=data_reg_predict_1990_2010)

reg_predict_7_politicalquality <- make_reg(
  as.formula(
    paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + ",
    "eci*GDP_pc_PPP_log + politicalquality + popgrowth + humancapital")
    ), 
  reg_data=data_reg_predict_1990_2010)

reg_predict_7_economicquality <- make_reg(
  as.formula(
    paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + ",
    "eci*GDP_pc_PPP_log + economicquality + popgrowth + humancapital")
    ), 
  reg_data=data_reg_predict_1990_2010)

reg_predict_7_politicalquality_oilexports <- make_reg(
  as.formula(
    paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + ", 
           "eci*GDP_pc_PPP_log + politicalquality + popgrowth + humancapital +", 
           " oilexports")
    ), 
  reg_data=data_reg_predict_1990_2010)

reg_predict_7_propertyrights <- make_reg(
  as.formula(
    paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + ", 
           "eci*GDP_pc_PPP_log + propertyrights + popgrowth + humancapital")
    ), 
  reg_data=data_reg_predict_1990_2010)

reg_predict_7_propertyrights_oilexports <- make_reg(
  as.formula(
    paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + ", 
           "eci*GDP_pc_PPP_log + propertyrights + popgrowth + humancapital + ", 
           "oilexports")
    ), 
  reg_data=data_reg_predict_1990_2010)

reg_predict_7_1970_1984 <- make_reg(
  as.formula(
    paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + eci*GDP_pc_PPP_log + ", 
           "kof_econ + popgrowth + humancapital")
  ), 
  reg_data=data_reg_predict_1970_1984)

reg_predict_7_developing <- make_reg(
  as.formula(
    paste0("avg_GDP_pc_PPP_growth ~ GDP_pc_PPP_log + kof_econ + ", 
           "eci*GDP_pc_PPP_log + popgrowth + humancapital")
    ), 
    reg_data=data_reg_predict_1985_2014_developing)

reg_panel_5year_7 <- make_panel_reg(
  as.formula(paste0(
    "GDP_pc_growth ~ lag(Penn_GDP_PPP_log, 1) + lag(eci,1) + kof_econ + ", 
    "lag(eci,1) * lag(Penn_GDP_PPP_log,1) + popgrowth + humancapital")),
  reg_data=regression_data_5_year, 
  panel_model="pooling")

reg_panel_5year_7_cfe <- make_panel_reg(
  as.formula(paste0(
    "GDP_pc_growth ~ lag(Penn_GDP_PPP_log, 1) + lag(eci,1) + kof_econ + ",
    "lag(eci,1) * lag(Penn_GDP_PPP_log,1) + popgrowth + humancapital")),
  reg_data=regression_data_5_year,
  panel_model="within", 
  panel_effect="individual")

# Table 2: Main econometric results (1985-2014, 108 countries)-----------------
stargazer(
  reg_predict_1[["reg"]], 
  reg_predict_4[["reg"]], 
  reg_predict_4_kof[["reg"]], 
  reg_predict_4_popgrowth[["reg"]], 
  reg_predict_4_humancapital[["reg"]], 
  reg_predict_7[["reg"]], 
  reg_panel_5year_7[["reg"]], 
  reg_panel_5year_7_cfe[["reg"]], 
  t = list(
    unlist(reg_predict_1[["tvals"]]), 
    unlist(reg_predict_4[["tvals"]]), 
    unlist(reg_predict_4_kof[["tvals"]]), 
    unlist(reg_predict_4_popgrowth[["tvals"]]), 
    unlist(reg_predict_4_humancapital[["tvals"]]), 
    unlist(reg_predict_7[["tvals"]]), 
    unlist(reg_panel_5year_7[["tvals"]]), 
    unlist(reg_panel_5year_7_cfe[["tvals"]])
    ), 
  se=list(
    unlist(reg_predict_1[["ses"]]), 
    unlist(reg_predict_4[["ses"]]), 
    unlist(reg_predict_4_kof[["ses"]]), 
    unlist(reg_predict_4_popgrowth[["ses"]]), 
    unlist(reg_predict_4_humancapital[["ses"]]), 
    unlist(reg_predict_7[["ses"]]), 
    unlist(reg_panel_5year_7[["ses"]]), 
    unlist(reg_panel_5year_7_cfe[["ses"]])
    ), 
  p=list(
    unlist(reg_predict_1[["pvals"]]), 
    unlist(reg_predict_4[["pvals"]]), 
    unlist(reg_predict_4_kof[["pvals"]]), 
    unlist(reg_predict_4_popgrowth[["pvals"]]), 
    unlist(reg_predict_4_humancapital[["pvals"]]), 
    unlist(reg_predict_7[["pvals"]]), 
    unlist(reg_panel_5year_7[["pvals"]]), 
    unlist(reg_panel_5year_7_cfe[["pvals"]])
    ),
  float = FALSE, out = here("output/tex/Tab2_mainresults_raw.tex"),
  column.labels = c("uncond. conv.", "ECI", "KOFecon", "POPgrowth", "HumanCapital", 
                    "All controls", "Panel-pooled", "country-FE"), 
  dep.var.caption = "", digits = 2, dep.var.labels.include = FALSE, 
  model.names = FALSE, omit.stat =  c("ser", "f"),
  order = c(1, 7, 4, 5, 6, 8),
  covariate.labels = c(
    "log(GDPpc)", "ECI", "Globalization", "Population growth", "Human capital",
    "log(GDPpc) $\\cdot$ ECI"),
  omit = "lag*"
  )

# Table 3: Robustness checks
stargazer(
  reg_predict_7[["reg"]], 
  reg_predict_7_developing[["reg"]], 
  reg_predict_7_1970_1984[["reg"]], 
  reg_predict_7_oilexports[["reg"]], 
  reg_predict_7_coalandmetalexports[["reg"]], 
  reg_predict_7_economicquality[["reg"]], 
  reg_predict_7_politicalquality[["reg"]], 
  reg_predict_7_legalquality[["reg"]], 
  reg_predict_7_politicalquality_oilexports[["reg"]], 
  t=list(
    unlist(reg_predict_7[["tvals"]]), 
    unlist(reg_predict_7_developing[["tvals"]]), 
    unlist(reg_predict_7_1970_1984[["tvals"]]), 
    unlist(reg_predict_7_oilexports[["tvals"]]), 
    unlist(reg_predict_7_coalandmetalexports[["tvals"]]),
    unlist(reg_predict_7_economicquality[["tvals"]]), 
    unlist(reg_predict_7_politicalquality[["tvals"]]), 
    unlist(reg_predict_7_legalquality[["tvals"]]), 
    unlist(reg_predict_7_politicalquality_oilexports[["tvals"]])
    ), 
  se=list(
    unlist(reg_predict_7[["ses"]]),
    unlist(reg_predict_7_developing[["ses"]]),
    unlist(reg_predict_7_1970_1984[["ses"]]),
    unlist(reg_predict_7_oilexports[["ses"]]),
    unlist(reg_predict_7_coalandmetalexports[["ses"]]),
    unlist(reg_predict_7_economicquality[["ses"]]),
    unlist(reg_predict_7_politicalquality[["ses"]]),
    unlist(reg_predict_7_legalquality[["ses"]]),
    unlist(reg_predict_7_politicalquality_oilexports[["ses"]]) 
    ), 
  p=list(
    unlist(reg_predict_7[["pvals"]]), 
    unlist(reg_predict_7_developing[["pvals"]]),
    unlist(reg_predict_7_1970_1984[["pvals"]]),
    unlist(reg_predict_7_oilexports[["pvals"]]), 
    unlist(reg_predict_7_coalandmetalexports[["pvals"]]), 
    unlist(reg_predict_7_economicquality[["pvals"]]), 
    unlist(reg_predict_7_politicalquality[["pvals"]]), 
    unlist(reg_predict_7_legalquality[["pvals"]]), 
    unlist(reg_predict_7_politicalquality_oilexports[["pvals"]])
    ), 
  out = here("output/tex/Tab3_robustness_raw.tex"), float = FALSE, 
  column.labels = c("baseline", "developing", "1970-1984", "oil", "coal", 
                    "econ inst", "pol inst", "leg inst", "leg + oil"), 
  dep.var.caption = "", digits = 3, dep.var.labels.include = FALSE, 
  model.names = FALSE, omit.stat =  c("ser", "f") #, omit="lag*",
  # covariate.labels = c(
  #   "log(GDPpc)", "Globalization", "ECI", "Oil exports", # "Coal exports",
  #   "Econ institutions", "Pol institutions", "Legal institutions", 
  #   "Population growth", "Human capital", "log(GDPpc) $\\cdot$ ECI")
  )

# Create figures 2 and 3-------------------------------------------------------
source(here("code/fig_2-3_margins.R"))

# Appendix ------

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