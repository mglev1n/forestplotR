% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scientific_notation.R
\name{scientific_notation}
\alias{scientific_notation}
\title{Reformat string as scientific notation}
\usage{
scientific_notation(l, n)
}
\arguments{
\item{l}{Character string to be formatted into scientific notation}

\item{n}{Number of digits to include prior to conversion to scientific notation}
}
\value{
A character string containing the input string transformed into scientific notation
}
\description{
\code{scientific_notation} is a function that takes a character string and returns a character string reformatted to scientific notation
}
\examples{
scientific_notation(0.00001, 3)

}
