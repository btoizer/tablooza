#' Format a descriptive statistics table
#' @param dat A data frame or tibble
#' @param dvs A vector of variables from dat for which descriptives will be conducted (dependent variables)
#' @param ivs An optional vector of variables from dat to split analyses on (independent variables). Defaults to NULL. Currently only supports 0, 1, or 2 values
#' @param na.rm Logical, indicates whether or not to remove missing data when calculating the mean and standard deviations. Defaults to FALSE.
#' @param reliability Logical, indicates whether or not to conduct reliability analyses (i.e. Cronbach's alpha) for each dependent variable, using {psych} package. To include reliability analyses, the columns that make up each dependent variable must be named the same as the dependent variable, followed by an underscore. Defaults to FALSE
#' @param pretty_rownames An optional vector with formatted dependent variable names.
#' @return Formatted descriptive statistics table
#' @export

desc_tab <- function(dat, dvs, ivs = NULL, na.rm = F, pretty_rownames = NULL, reliability = F, sep = ""){

  # Define function to format individual mean/sd combinations
  format_mean_sd <- function(x, na.rm = F){
    if(na.rm == F){
      mean_val <- round(mean(x, na.rm = F), 2)
      sd_val <- round(stats::sd(x, na.rm = F), 2)
    } else {
      mean_val <- round(mean(x, na.rm = T), 2)
      sd_val <- round(stats::sd(x, na.rm = T), 2)
    }
    formatted_val <- paste0(mean_val, " (", sd_val, ")")
    return(formatted_val)
  }

  # Define function to create a vector of formatted mean/sd combinations
  construct_col <- function(dat, dvs, na.rm = F){
    dat <- data.frame(dat)
    new_col <- c()
    for(i in 1:length(dvs)){
      cur_var <- dvs[i]
      cur_vec <- dat[,cur_var]
      new_col <- c(new_col, format_mean_sd(cur_vec, na.rm=na.rm))
    }
    return(new_col)
  }

  # Unfortunately, function can only deal with 2 IVs at the moment
  if(length(ivs ) > 2){
    stop("Function currently only supports up to 2 independent variables.")
  }

  # Make sure dat is a data frame
  dat <- data.frame(dat)

  # Create varnames vector that will be first column of output table
  if(is.null(pretty_rownames)){
    # If not provided, use variable/column names
    varnames <- dvs
  } else{
    # If provided, use vector of pretty names
    varnames <- pretty_rownames
  }

  # Create a new data frame using varnames
  desc_tab <- data.frame(varnames, stringsAsFactors = F)
  names(desc_tab) <- ""

  # If Cronbach's alpha are requested
  if(reliability == T){

    # Use psych package to calculate alphas
    alpha_vec <- c()
    for(stem in dvs){
      stem <- paste0(stem, sep) # Variables must be formatted as stem_ to work
      scale_vars <- dat %>% dplyr::select(tidyselect::starts_with(stem))
      full_reliability <- psych::alpha(scale_vars)
      cur_alpha <- full_reliability$total$raw_alpha # Save only unstandardized alpha estimate
      alpha_vec <- c(alpha_vec, cur_alpha)
    }

    # Add to table
    alpha_vec <- round(alpha_vec, 2)
    desc_tab[2] <- alpha_vec
    names(desc_tab)[2] <- "Cronbach's Alpha"
  }

  # If there are no IVs to split on, then just find totals, add to table
  if(is.null(ivs)){
    total_col <- construct_col(dat = dat, dvs = dvs, na.rm = na.rm)
    desc_tab[,(length(desc_tab) + 1)] <- total_col
    names(desc_tab)[length(desc_tab)] <-  "Mean (SD)"
  } else

    # If there is just one IV to split on
    if(length(ivs) == 1) {

      # Save vector of each condition/level of the IV
      conditions <- unique(dat[ivs])

      # Save number of levels of the IV
      nlevels <- nrow(conditions)

      # Save current length of table
      cur_len <- length(desc_tab)

      # Fill data frame with one column per condition
      for(i in 1:nlevels){
        cur_val <- as.character(unique(dat[,ivs])[i])
        cur_df <- dat[dat[ivs] == cur_val,]
        cur_col <- construct_col(cur_df, dvs = dvs, na.rm = na.rm)
        desc_tab[,(i + cur_len)] <- cur_col
        names(desc_tab)[(i + cur_len)] <- paste(ivs, cur_val, sep = ".")
      }

      # Calculate column for total sample and add to data frame
      total_col <- construct_col(dat = dat, dvs = dvs, na.rm = na.rm)
      desc_tab[,(length(desc_tab) + 1)] <- total_col
      names(desc_tab)[length(desc_tab)] <- "Total"
    }

  # If there are two IVs to split on
  if(length(ivs) == 2) {

    # Add blank row for sub-column names
    desc_tab <- rbind(rep("", length(desc_tab)), desc_tab)

    # Save vector of level of each IV
    conditions1 <- unique(dat[ivs[1]])
    conditions2 <- unique(dat[ivs[2]])

    # Save number of levels of the IVs
    nlevels1 <- nrow(conditions1)
    nlevels2 <- nrow(conditions2)

    # Loop through first IV to calculate, add to table
    for(i in 1:nlevels1){
      cur_val1 <- as.character(unique(dat[,ivs[1]])[i])
      cur_df <- dat[dat[ivs[1]] == cur_val1,]

      # Loop through second IV to calculate, add to table
      for(i2 in 1:nlevels2){
        cur_val2 <- as.character(unique(dat[,ivs[2]])[i2])
        cur_df2 <- cur_df[cur_df[ivs[2]] == cur_val2,]
        cur_col <- construct_col(cur_df2, dvs = dvs, na.rm = na.rm)
        cur_label <- paste(ivs[2], cur_val2, sep = ".")
        cur_col <- c(cur_label, cur_col)
        desc_tab[,(1 + length(desc_tab))] <- cur_col
      }
      cur_col <- construct_col(cur_df, dvs = dvs, na.rm = na.rm)
      cur_label <- paste(ivs[2], "total", sep = ".")
      cur_col <- c(cur_label, cur_col)
      desc_tab[,(1 + length(desc_tab))] <- cur_col
    }

    # Calculate column for total sample and add to data frame
    total_col <- construct_col(dat = dat, dvs = dvs, na.rm = na.rm)
    total_col <- c("", total_col)
    desc_tab[,(length(desc_tab) + 1)] <- total_col

    # Rename columns
    names_vec <- ifelse(reliability == T, c("", "Cronbach's Alpha"), c(""))
    for(i in 1:nlevels1){
      cur_name <- paste(ivs[1], as.character(unique(dat[,ivs[1]])[i]), sep = ".")
      names_vec <- c(names_vec, cur_name, rep("", nlevels2))
    }
    names_vec <- c(names_vec, "Total")
    names(desc_tab) <- names_vec
  }

  # Return table
  return(desc_tab)
}
