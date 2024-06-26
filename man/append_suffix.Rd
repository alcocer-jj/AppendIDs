% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AppendIDfunctions.R
\name{append_suffix}
\alias{append_suffix}
\title{Append Suffixes}
\usage{
append_suffix(df, suffix, dyad = FALSE)
}
\arguments{
\item{df}{A country-year dataframe with either monadic or dyadic country-year observations.}

\item{suffix}{A character string to append as an suffix to all non-identifying variable columns, such as "COW" for Correlates of War.}
}
\value{
A monadic or dyadic country-year dataframe with variable column names modified with the provided suffix.
}
\description{
Appends a designated character string ("suffix") to all variables in a given dataframe except for identifying column variables. Identifying column variables include country names, identifiers, and abbreviations, and year.
This function is designed to allow researchers to append suffixes to differentiate between variables in a combined dataset based on their dataset of origin.
For example, "COW" could be appended as a suffix to variables from the Correlates of War dataset.
}
\examples{
data_suffix <- append_suffix(data, " ")
}
