% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RejectionSample.R
\name{rejectionsample}
\alias{rejectionsample}
\title{Rejection Sampling}
\usage{
rejectionsample(n, pdf, a, b, C)
}
\arguments{
\item{n}{The number of samples}

\item{pdf}{A function that is the pdf of the random variable that you wish to sample from}

\item{a}{a numeric that is the lower bound of the random variable you wish to sample from}

\item{b}{a numeric that is the upper bound of the random variable you with to sample from}

\item{C}{a numeric that is such that f(x) <= C for all values of x}
}
\value{
Samples of length n from the given pdf
}
\description{
This is a function that peforms rejection sampling from a given pdf
}
