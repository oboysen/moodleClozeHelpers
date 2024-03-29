% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moodleClozeHelpers.R
\name{print_metainfo}
\alias{print_metainfo}
\title{print_metainfo}
\usage{
print_metainfo(cl)
}
\arguments{
\item{cl}{clozeL question container object}
}
\description{
Collect and print as markdown all meta-information from a clozeL object.
}
\examples{
data('mtcars')
cl <- new_clozeL('Car quiz')
# Question: How many cylinders does a Datsun 710 have?
cl <- add_schoice(cl, options=sort(unique(mtcars$cyl)), correctno=1, points=1, shuffle=FALSE)
print_metainfo(cl)
}
