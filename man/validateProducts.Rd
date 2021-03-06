% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/files.R
\name{validateProducts}
\alias{validateProducts}
\title{Validate input file with products}
\usage{
validateProducts(file = "products.csv")
}
\arguments{
\item{file}{path to the file with products list}
}
\value{
Function returns a list with two elements. Frst element is a logical variable with
the result of validation. If the validation fails the second element on the list is the message
describing what was wrong. If the validation was successfull then the second element will be
a data.frame with products read from file.
}
\description{
Function verifies the content of a file which should contain a list of products.
It verifies if the file contains 3 rqeauired columns ('Item', 'Price' and 'SpecialPrice'),
if the Price column has numeric values and if the format of SpecialPrice column is correct.
}
