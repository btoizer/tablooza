% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/desc_tab.R
\name{desc_tab}
\alias{desc_tab}
\title{Format a descriptive statistics table}
\usage{
desc_tab(
  dat,
  dvs,
  ivs = NULL,
  na.rm = F,
  pretty_rownames = NULL,
  reliability = F,
  sep = ""
)
}
\arguments{
\item{dat}{A data frame or tibble}

\item{dvs}{A vector of variables from dat for which descriptives will be conducted (dependent variables)}

\item{ivs}{An optional vector of variables from dat to split analyses on (independent variables). Defaults to NULL. Currently only supports 0, 1, or 2 values}

\item{na.rm}{Logical, indicates whether or not to remove missing data when calculating the mean and standard deviations. Defaults to FALSE.}

\item{pretty_rownames}{An optional vector with formatted dependent variable names.}

\item{reliability}{Logical, indicates whether or not to conduct reliability analyses (i.e. Cronbach's alpha) for each dependent variable, using {psych} package. To include reliability analyses, the columns that make up each dependent variable must be named the same as the dependent variable, followed by an underscore. Defaults to FALSE}

\item{sep}{Denotes the character separating variable stems and item numbers in the data frame, in order to select the relevant variables to calculate Cronbach's alpha. Only used when reliability = T}
}

\value{
Formatted descriptive statistics table
}
\description{
Format a descriptive statistics table
}
