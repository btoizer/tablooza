#' Find eta squared and partial eta squared for analysis of variance.
#' @param x A linear model object created with lm().
#' @param type A number 2 or 3 to indicate the type of sums of squares to use in the analysis.
#' @param anova Logical, indicating whether or not to include full ANVOA output with effect size.
#' @return Eta squared and partial eta squared, ANOVA table if requested
#' @export

etaSquared<- function(x, type = 3, anova = FALSE ) {

  # Function written by Dan Navarro, but currently unpublished
  # https://github.com/djnavarro/lsr/blob/master/R/etaSquared.R

if( !methods::is(anova,"logical") | length(anova) !=1 ) {
  stop( '"anova" must be a single logical value')
}
if( !methods::is(x,"lm") ) {stop( '"x" must be a linear model object')}
if( !methods::is(type,"numeric") | length(type) !=1 ) {
  stop("type must be equal to 1,2 or 3")
}

if( type == 1) {

  ss <- anova(x)[,"Sum Sq",drop=FALSE]  # Type 1 SS
  ss.res <- ss[dim(ss)[1],]  # Full model RSS
  ss.tot <- sum( ss )  # Total SS
  ss <- ss[-dim(ss)[1],,drop=FALSE]
  ss <- as.matrix(ss) # later code assumes ss is a matrix

} else { if (type == 2) {

  # get the residual and total sum of squares
  ss.tot <- sum(( x$model[,1] - mean(x$model[,1]) )^2)
  ss.res <- sum(( x$residuals)^2)

  # get information about how terms depend on variables (1st row is the DV, so drop it)
  terms <- attr(x$terms,"factors")[-1,,drop=FALSE]

  # initialise the ss matrix
  l <- attr(x$terms,"term.labels")
  ss <- matrix(NA,length(l),1)
  rownames(ss) <- l

  # compute ss values
  for( i in seq_along(ss) ) {

    # what variables does this term depend on?
    vars.this.term <- which( terms[,i] != 0 )

    # which terms are dependent on this term?
    dependent.terms <- which( apply( terms[ vars.this.term,,drop=FALSE], 2, prod )>0 )

    # null model removes all of the dependent terms
    m0 <- stats::lm( x$terms[-dependent.terms], x$model )  # remove all of these

    # terms with higher order terms need a separate alternative model...
    if( length(dependent.terms)>1 ) {
      m1 <- stats::lm( x$terms[-setdiff(dependent.terms,i)], x$model ) # remove all except i-th term
      ss[i] <- anova(m0,m1)$`Sum of Sq`[2] # get the ss value

      # terms without higher order dependent terms can be directly compared to the full model...
    } else {
      ss[i] <- anova(m0,x)$`Sum of Sq`[2]
    }
  }


} else { if (type == 3) {

  mod <- stats::drop1(x,scope=x$terms)
  ss <- mod[-1,"Sum of Sq",drop=FALSE] # Type 3 SS
  ss.res <- mod[1,"RSS"] # residual SS
  ss.tot <- sum(( x$model[,1] - mean(x$model[,1]) )^2)
  ss <- as.matrix(ss) # later code assumes ss is a matrix

} else {
  stop("type must be equal to 1,2 or 3")
}}}

# output matrix if anova not requested...
if( anova == FALSE) {
  eta2 <- ss / ss.tot
  eta2p <- ss / (ss + ss.res)
  E <- cbind(eta2, eta2p)
  rownames(E) <- rownames(ss)
  colnames(E) <- c("eta.sq","eta.sq.part")

  # output matrix if anova is requested...
} else {
  ss <- rbind( ss, ss.res )
  eta2 <- ss / ss.tot
  eta2p <- ss / (ss + ss.res)
  k <- length(ss)
  eta2p[k] <- NA
  df <- anova(x)[,"Df"]
  ms <- ss/df
  Fval <- ms / ms[k]
  p <- 1- stats::pf( Fval, df, rep.int(df[k],k) )
  E <- cbind(eta2, eta2p, ss, df, ms, Fval, p)
  E[k,6:7] <- NA
  colnames(E) <- c("eta.sq","eta.sq.part","SS","df","MS","F","p")
  rownames(E) <- rownames(ss)
  rownames(E)[k] <- "Residuals"
}
return(E)

}
