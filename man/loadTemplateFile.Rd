% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qa4Seas.helpers.R
\name{loadTemplateFile}
\alias{loadTemplateFile}
\title{Read template files}
\usage{
loadTemplateFile(file)
}
\arguments{
\item{file}{Which parameter file to read? Possible values are
\code{"datasets"}, \code{"variables"} or \code{"products"}.}
}
\value{
A \code{data.frame}
}
\description{
Read the template csv files in inst folder containing pre-defined metadata
}
\keyword{internal}
