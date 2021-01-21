#' Format linear regression table
#' @param mod A linear model object created with lm().
#' @param preds_vec A vector of strings indicating variable names. Strings do not need to match column names, but do need to be in the same order as the independent variables in the call for mod.
#' @param betas "unstd" or "std" Indicates if you want unstandardized coefficients or standardized beta coefficients. Defaults to c("unstd", "std").
#' @param se Logical, indicates if you want standard errors. Defaults to TRUE.
#' @return Formatted regression table
#' @export

reg_tab <- function(mod, preds_vec, betas = c("unstd", "std"), se = T){

  # create data frame with just predictor names
  reg_tab <- data.frame(predictors = preds_vec)

  # take coefficients from model
  tab <- summary(mod)$coefficients

  # for unstandardized beta estimates in output
  if(any(betas == "unstd")){

    # for loop to find beta values, determine p-value, and add stars if applicable
    vec_out <- c()
    for(i in c(2:nrow(tab))){
        vec_out[i] <- signif_stars(tab[i,1], tab[i,4])
    }

    # save starred values into vector
    unstd_beta_vals <- vec_out

    # add to table
    reg_tab[, (length(reg_tab)+1)] <- unstd_beta_vals
    names(reg_tab)[length(reg_tab)] <- "unstd.beta"
  }

  # for standardized beta estimates in output
  if(any(betas =="std")){

    # Use standardization function from QuantPsyc to get standardized values
    tab_std <- data.frame(QuantPsyc::lm.beta(mod))
    names(tab_std) <- "std.beta"
    std.beta_vec <- c(NA,tab_std$std.beta)
    vec_out <- c()

    # for loop to add stars if applicable
    for(i in c(2:nrow(tab))){
      vec_out[i] <- signif_stars(std.beta_vec[i], tab[i,4])
    }
    std_beta_vals <- vec_out

    # add to table
    reg_tab[, (length(reg_tab)+1)] <- std_beta_vals
    names(reg_tab)[length(reg_tab)] <- "std.beta"
  }

  # for standard error values in output
  if(se == T){

    # save standard errors into a vector
    tab <- data.frame(tab)
    std_err_vals <- round(tab[2:nrow(tab),2], 3)

    # add to table
    reg_tab[, (length(reg_tab)+1)] <- std_err_vals
    names(reg_tab)[length(reg_tab)] <- "SE"
  }
  # return table
  return(reg_tab)
}



