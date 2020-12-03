#' Add significance stars to an estimate
#' @param estimate A point estimate or statistic, should be a single numeric value.
#' @param p_value A p-value from a statistical test, should be a single numeric value between 0 and 1.
#' @return The estimate with the appropriate number of stars, depending on level of significance.
#' @export

signif_stars <- function(estimate, p_value){
  if(p_value > 1){
    message("Warning: p values should not be greater than 1.")
  }

  if(p_value < .10 & p_value > .05){                                    # marginal
    return(paste(round(estimate, 2), "+", sep = ""))} else{
      if(p_value < .05 & p_value > .01){                                # less than .05
        return(paste(round(estimate, 2), "*", sep = ""))} else{
          if(p_value < .01 & p_value > .001){                           # less than .01
            return(paste(round(estimate, 2), "**", sep = ""))} else{
              if(p_value < .001){                                       # less than .001
                return(paste(round(estimate, 2), "***", sep = ""))} else{
                  return(round(estimate, 2))}                            # not significant
            }}}}
