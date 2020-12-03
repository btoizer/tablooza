#' Format an analysis of variance table
#' @param mod A linear model object created with lm().
#' @param type A number 2 or 3 to indicate the type of sums of squares to use in the analysis. Defaults to 3.
#' @param pretty_names An optional vector to replace column names with formatted variable names.
#' @return Formatted ANOVA table
#' @export


anova_tab <- function(mod, type = 3, pretty_names = NULL){

  # Check that x is an lm() object and type is 1, 2, 3
  if( !methods::is(mod,"lm") ) {stop( '"mod" must be a linear model object')}
  if( type != 2 & type != 3 ) {
    stop("type must be equal to 2 or 3")}

  # Convert lm model to car Anova object
  mod_anova <- car::Anova(mod, type = type)

  # Create vector of f-values with significance stars
  f_stars <- c()
  for(i in 1:nrow(mod_anova)){
    f_val <- mod_anova$`F value`[i]
    p_val <-  mod_anova$`Pr(>F)`[i]
    format_val <- ifelse(is.na(f_val), "", signif_stars(f_val, p_val))
    f_stars <- c(f_stars, format_val)
  }

  # Find eta-squared values for model
  eta_sq <- etaSquared(mod, type = type)

  # Create vector of variable names
  if(is.null(pretty_names)){
    varnames = rownames(mod_anova)
  } else{
    if(type == 2){
      varnames = c(pretty_names, "Residuals")
    } else{
      varnames = c("Intercept", pretty_names, "Residuals")
    }
  }

  # Format eta values, if type III, then account for extra row for intercept
  eta.sq <- as.character(round(eta_sq[,1], 3))
  eta.sq.part <- as.character(round(eta_sq[,2], 3))
  if(type == 3){
   eta.sq <- c("-", eta.sq, "")
   eta.sq.part <- c("-", eta.sq.part, "")
  } else{
    eta.sq <- c(eta.sq, "")
    eta.sq.part <- c(eta.sq.part, "")
    }

  # Put values into a new data frame
  anova_tab <- data.frame(
    varnames = varnames,
    df = mod_anova$Df,
    SS = round(mod_anova$`Sum Sq`, 2),
    F.val = f_stars,
    eta.sq = eta.sq,
    eta.sq.part = eta.sq.part
  )

  # Reformat names of data frame
  names(anova_tab) <- c("", "*df*", "*SS*", "*F*", "*$\\eta^2$*", "partial *$\\eta^2$*")

  # And return table
  return(anova_tab)
}
