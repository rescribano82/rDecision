% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/criteria.R
\name{dec.laplace}
\alias{dec.laplace}
\title{Laplace criteria}
\usage{
dec.laplace(m)
}
\arguments{
\item{m}{matrix with strategies in rows and states in columns. Benefits in 
positive and costs in negative}
}
\value{
A list with best Laplace criteria decision and means vector
}
\description{
Laplace criteria
}
\examples{
#
m <- data.frame(c(200, 260, 272), 
                c(300, 320, 280), 
                c(340, 320, 300))
dimnames(m) <- list(LETTERS[1:nrow(m)], 
                    paste0('State_', as.roman(1:ncol(m))))
dec.laplace(m)
}
\author{
Ruben Escribano-Garcia \email{escribano.engineer@gmail.com}
}
