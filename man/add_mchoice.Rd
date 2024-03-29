% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moodleClozeHelpers.R
\name{add_mchoice}
\alias{add_mchoice}
\title{add_mchoice}
\usage{
add_mchoice(cl, options, correctno, pointfrac = NULL, points = 1,
  shuffle = FALSE, display = "vertical")
}
\arguments{
\item{cl}{clozeL container object.}

\item{options}{MCQ answer options provided as a character vector.}

\item{correctno}{A numerical vector of numbers indicating the positions of the correct answer options.}

\item{pointfrac}{A numeric vector of point fractions given for each answer option. If not provided, fractions will be calculated such that all correct answers.
together weight 100\% and all incorrect answer together weight -100\%. Note that Moodle does not allow any decimal places for the point fractions here (found out by testing).}

\item{points}{Maximum points for this question.}

\item{shuffle}{Shuffle answer options randomly.}

\item{display}{The arrangement of the tick boxes, a string "vertical" or "horizontal".}
}
\value{
returns a \code{clozeL} question container object with the schoice question added.
}
\description{
Add a question real multiple choice questions where multiple responses can be selected
(exams package: "mchoice", Moodle: "MULTIRESPONSE") to a \code{\link{clozeL}} container object of questions
}
\examples{
data('mtcars')
cl <- new_clozeL('Car quiz')
# Question: Which of the following cars get more than 20 miles out of a galon?
add_mchoice(cl, options=rownames(mtcars)[1:6], correctno=c(1:4), points=1, shuffle=FALSE)

}
\seealso{
\code{\link{clozeQ}}
}
