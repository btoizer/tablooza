#' Format a bivariate correlation table
#' @param x A data frame, matrix, or tibble containing only numeric columns to be included in the bivariate correlations.
#' @param pretty_names An optional vector with formatted dependent variable names.
#' @param num_cols Logical, when TRUE numbers each variable and replaces column names with corresponding numbers. Defaults to FALSE.
#' @return Formatted bivariate correlation table
#' @export

corr_tab <- function(x, pretty_names = NULL, num_cols = F){

  # Function adapted from function on blog post
  # http://myowelt.blogspot.com/2008/04/beautiful-correlation-tables-in-r.html

  # Make sure x is a matrix
  x <- as.matrix(x)
  R <- Hmisc::rcorr(x)$r
  p <- Hmisc::rcorr(x)$P

  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", ifelse(p < .10, "+", " "))))

  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]

  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")

  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew)

  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])

  ## change variable names to pretty names if provided
  if(!is.null(pretty_names)){
    rownames(Rnew) <- pretty_names
    colnames(Rnew) <- pretty_names[1:(length(pretty_names) - 1)]
  }

  ## if using numbering for columns rather than variable names
  if(num_cols == T){
    rownames(Rnew) <- paste(1:length(rownames(Rnew)), rownames(Rnew), sep = " ")
    colnames(Rnew) <- 1:length(colnames(Rnew))
  }

  return(Rnew)
}

