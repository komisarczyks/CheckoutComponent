% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/files.R
\name{readDiscounts}
\alias{readDiscounts}
\title{Read discounts from file}
\usage{
readDiscounts(products, file = "discounts.csv")
}
\arguments{
\item{file}{path to the file with products list}
}
\value{
Function returns a data.frame with discounts read from file if the validation
was successfull.
}
\description{
Function runs a validation of a supplied file containing discounts and throws an error
if it was not successfull.
}
