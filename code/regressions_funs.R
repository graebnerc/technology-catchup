
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