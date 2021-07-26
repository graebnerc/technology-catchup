
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
make_reg <- function(reg_formula, reg_data, get_marg_plot=FALSE, ...){
  
  reg_object <- lm(formula = reg_formula, data=reg_data, na.action=na.exclude)
  
  if (isFALSE(get_marg_plot)){
    ses <- list(
      coeftest(reg_object, vcov.=function(x) vcovHC(x))[,2]) 
    tvals <- list(
      coeftest(reg_object, vcov.=function(x) vcovHC(x))[,3]) 
    pvals <- list(
      coeftest(reg_object, vcov.=function(x) vcovHC(x))[,4]) 
    
    marg_obj <- margins(lm(formula = reg_formula, data = reg_data))
    
    right_side_vars <- labels(
      terms(reg_formula))[1:length(labels(terms(reg_formula)))-1]
    
    mem_marg_obj <- margins(
      model = lm(formula = reg_formula, data = reg_data), 
      at = as.list(apply(reg_data[right_side_vars], 2, mean, na.rm=TRUE))
    )
    
    final_list <- list(
      "reg" = reg_object,
      "marg" = marg_obj,
      "mem_marg" = mem_marg_obj,
      "ses" = ses,
      "tvals" = tvals,
      "pvals" = pvals
    )
  } else {
    dot_args <- list(...)
    marg_plot_dx <- dot_args[["marg_plot_dx"]]
    marg_plot_x <- dot_args[["marg_plot_x"]]
    marg_plot_xvals <- dot_args[["marg_plot_xvals"]]
    # marg_plot_dx <- "eci"
    # marg_plot_x <- "GDP_pc_PPP_log"
    # marg_plot_xvals <- seq(6, 11, 0.1)
    marg_plot_data <- cplot(
      object = reg_object, 
      dx = marg_plot_dx, 
      x = marg_plot_x, 
      data = reg_data,  
      what = "effect", 
      draw = FALSE, 
      xvals = marg_plot_xvals
      )
    marg_plot_data <- tibble(marg_plot_data) %>%
      dplyr::mutate(years=paste0(
        dot_args[["first_year"]], "-", dot_args[["last_year"]]))
    return(marg_plot_data)
  }
  
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