#' Format hierarchical linear regression table
#' @param mod_steps A list of linear model objects created with lm(), in the order of analysis.
#' @param preds_vec A vector of strings indicating variable names. Strings do not need to match column names, but do need to be in the same order as the independent variables in the call for mod.
#' @param betas "unstd" or "std" Indicates if you want unstandardized coefficients or standardized beta coefficients. Defaults to c("unstd", "std").
#' @param se Logical, indicates if you want standard errors. Defaults to TRUE.
#' @return Formatted hierarchical regression table
#' @export

hreg_tab <- function(mod_steps, preds_vec, betas = "unstd", se = T){

  # Create data frame with predictor names
  hreg_tab <- data.frame(predictors = preds_vec, stringsAsFactors = F)

  # Determine the number of columns per regression step needed
  ncols <- ifelse(se == T, (length(betas) + 1), length(betas))

  # Create new vector for adjusted r squared values
  rsq <- c("Adjusted R^2")

  # Loop through model steps, find regression output, add to table, and save r squared
  for(i in 1:length(mod_steps)){
    cur_pred_len <-length(names(mod_steps[[i]]$model)[-1])
    cur_preds_vec <- preds_vec[1:cur_pred_len]
    cur_tab <- reg_tab(mod = mod_steps[[i]], cur_preds_vec, betas = betas, se = se)
    hreg_tab <- dplyr::left_join(hreg_tab, cur_tab, by = "predictors")

    cur_summary <- summary(mod_steps[[i]])
    cur_rsq <- round(cur_summary$r.squared, 2)
    rsq <- c(rsq, cur_rsq, rep("", (ncols - 1)))
  }

  # Add r squared values to table
  hreg_tab <- rbind(hreg_tab, rsq)

  # Save current variable names (statistic labels) as first row of data frame
  hreg_tab <- rbind(names(hreg_tab), hreg_tab)

  # Save vector of the step number, with blanks based on number of columns
  new_names <- c("")
  for(i in 1:length(mod_steps)){
    cur_name <- paste("Step", i, sep = " ")
    new_names <- c(new_names, cur_name, rep("", (ncols-1)))
  }

  # Change names to denote step number
  names(hreg_tab) <- new_names

  # Make all NAs blanks
  hreg_tab[is.na(hreg_tab)] <- ""

  # Return table
  return(hreg_tab)
}
