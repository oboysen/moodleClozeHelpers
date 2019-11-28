# moodleClozeHelpers


#' clozeQ
#' @description Create an object for a single question as part of a cloze question
#' @param answer the correct answer always given as the first options, additional answer options might be added, separated by a wiggle symbol "~". For \code{schoice} questions, this is ignored and the information from \code{mcq_correctno} is used.
#' @param type one of num, string, schoice
#' @param points maximum points for this question.
#' @param pointfrac points fraction given for answer options. For first answer, it must be 100, for following options a percentage between 0 and 100 may be provided, separated by "~".
#' @param tol tolerance for numerical questions.
#' @param mcq_options MCQ answer options provided as a character vector.
#' @param mcq_correctno Number indicating the correct MCQ answer option.
#' @param mcq_shuffle Shuffle multiple or single choice answer options randomly.
#' @param mchoice_display In multiple choice questions (not single choice!), the arrangement of the tick boxes, a string "vertical" or "horizontal".
#'
#' @return returns a \code{clozeQ} question object.
#' @export
#'
clozeQ <- function(answer, type='num', points=1, pointfrac=100, tol=0.001, mcq_options='', mcq_correctno=NULL, mcq_shuffle=FALSE, mchoice_display='') {
  if (!is.null(mcq_correctno)) {
    qa <- rep(0,length(mcq_options))
    qa[mcq_correctno] <- 1
    if (mcq_shuffle) {
      idx <- sample(1:length(qa), replace=FALSE, size=length(qa))
    } else {
      idx <- 1:length(qa)
    }
    answer <- paste0(qa[idx], collapse='')
    mcq_options <- mcq_options[idx]
  }
  if (length(answer)>1) {
    answer <- paste0(answer, collapse='~')
    tol <- paste0(tol, collapse='~')
  }
  if (length(pointfrac)>1) {
    pointfrac <- paste0(pointfrac, collapse='~')
  }
  obj <- list(answer=answer, type=type, points=points, pointfrac=pointfrac, tol=tol, mcq_options=mcq_options, mchoice_display=mchoice_display)
  attr(obj, 'class') <- 'clozeQ'
  return(obj)
}

#' clozeL
#' @description Create a \code{clozeL} container object to store the partial question \code{clozeQ} object for a cloze question
#' @param name name of the cloze question for meta-information
#'
#' @return returns a \code{clozeL} question container object.
#'
clozeL <- function(name) {
  obj <- list()
  attr(obj, 'class') <- 'clozeL'
  attr(obj, 'Qname') <- name
  return(obj)
}

#' new_clozeL
#' @description Create a new \code{clozeL} container object to store the partial question \code{clozeQ} object for a cloze question.
#' @param name A string, name of the cloze question for meta-information.
#'
#' @return returns a \code{clozeL} question container object.
#' @export
#'
#' @examples new_clozeL('Quiz 1')
new_clozeL <- function(name) {
  clozeL(name)
}


#' add_schoice
#' @description Add a single choice question (exams package: "schoice", Moodle: "MULTICHOICE") to a \code{\link{clozeL}} container object of questions.
#'
#' @param cl clozeL container object.
#' @param options MCQ answer options provided as a character vector.
#' @param correctno A single numerical, indicates the correct MCQ answer option.
#' @param points Numerical, the maximum points for this question.
#' @param shuffle Shuffle answer options randomly.
#'
#' @return returns a \code{clozeL} question container object with the schoice question added.
#' @export
#'
#' @examples
#' data('mtcars')
#' cl <- new_clozeL('Car quiz')
#' # Question: How many cylinders does a Datsun 710 have?
#' add_schoice(cl, options=sort(unique(mtcars$cyl)), correctno=1, points=1, shuffle=FALSE)
#'
#' @seealso \code{\link{clozeQ}}
add_schoice <- function(cl, options, correctno, points=1, shuffle=FALSE) {
  add_clozeQ(cl, type='schoice', mcq_options=options, mcq_correctno=correctno, points=points, mcq_shuffle=shuffle)
}

#' add_mchoice
#' @description Add a question real multiple choice questions where multiple responses can be selected
#' (exams package: "mchoice", Moodle: "MULTIRESPONSE") to a \code{\link{clozeL}} container object of questions
#'
#' @param cl clozeL container object.
#' @param options MCQ answer options provided as a character vector.
#' @param correctno A numerical vector of numbers indicating the positions of the correct answer options.
#' @param pointfrac A numeric vector of point fractions given for each answer option. If not provided, fractions will be calculated such that all correct answers.
#' together weight 100\% and all incorrect answer together weight -100\%. Note that Moodle does not allow any decimal places for the point fractions here (found out by testing).
#' @param points Maximum points for this question.
#' @param shuffle Shuffle answer options randomly.
#' @param display The arrangement of the tick boxes, a string "vertical" or "horizontal".
#'
#' @return returns a \code{clozeL} question container object with the schoice question added.
#' @export
#'
#' @examples
#' data('mtcars')
#' cl <- new_clozeL('Car quiz')
#' # Question: Which of the following cars get more than 20 miles out of a galon?
#' add_mchoice(cl, options=rownames(mtcars)[1:6], correctno=c(1:4), points=1, shuffle=FALSE)
#'
#' @seealso \code{\link{clozeQ}}
add_mchoice <- function(cl, options, correctno, pointfrac=NULL, points=1, shuffle=FALSE, display='vertical') {
  if (is.null(pointfrac)) {
    n_on <- length(correctno)
    n_off <- length(options) - n_on
    pointfrac <- rep((-100)/n_off, length(options))
    pointfrac[correctno] <- 100/n_on
  }
  if (display=='vertical') {
    mchoice_display <- 'MULTIRESPONSE'
  } else {
    mchoice_display <- 'MULTIRESPONSE_H'
  }
  add_clozeQ(cl, type='mchoice', mcq_options=options, mcq_correctno=correctno, pointfrac=pointfrac, points=points, mcq_shuffle=shuffle, mchoice_display=mchoice_display)
}

#' add_num
#' @description Add a fill-in-a-number question (exams package: "num", Moodle: "NUMERICAL")
#' to a \code{\link{clozeL}} container object of questions
#'
#' @param cl clozeL container object
#' @param answer the correct answer always given as the first options, additional answer options
#' might be added, separated by a wiggle symbol "~".
#' @param points maximum points for this question.
#' @param pointfrac Numeric vector of points fractions given for answer options. The first answer
#' must be correct and have 100, following options fraction between 0 and 100.
#' @param tol tolerance for numerical questions.
#'
#' @return returns a \code{clozeL} question container object with the num question added.
#' @export
#'
#' @examples
#' data('mtcars')
#' cl <- new_clozeL('Car quiz')
#' # Question: How many cylinders does a Ferrari Dino have?
#' add_num(cl, answer=6, points=1, tol=1)
#' # Question: How many mpg does a Datsun 710 make?
#' add_num(cl, answer=c(22.8,22.8), points=2, tol=c(1,4), pointfrac=c(100,50))
#' @seealso \code{\link{clozeQ}}
add_num <- function(cl, answer, points=1, pointfrac=100, tol=0.001) {
  add_clozeQ(cl, type='num', answer=answer, points=points, pointfrac=pointfrac, tol=tol)
}

#' add_string
#' @description Add a fill-in-a-string question (exams package: "string", Moodle: "SHORTANSWER") to a \code{\link{clozeL}} container object of questions
#'
#' @param cl clozeL container object
#' @param answer the correct answer always given as the first options, additional answer options might be added, separated by a wiggle symbol "~".
#' @param points maximum points for this question.
#' @param pointfrac Numeric vector of points fractions given for answer options. The first answer must be correct and have 100, following options fraction between 0 and 100.
#'
#' @return returns a \code{clozeL} question container object with the num question added.
#' @export
#'
#' @examples
#' data('mtcars')
#' cl <- new_clozeL('Car quiz')
#' # Question: Which of the cars has the most horsepower?
#' add_string(cl, answer=c('Maserati Bora','Camaro Z28','Duster 360'), points=2,
#' pointfrac=c(100,50,50))
#' @seealso \code{\link{clozeQ}}
add_string <- function(cl, answer, points=1, pointfrac=100) {
  add_clozeQ(cl, type='string', answer=answer, points=points, pointfrac=pointfrac)
}


#' add_clozeQ
#' @description Add a question to a \code{\link{clozeL}} container object of questions
#'
#' @param cl clozeL container object
#' @param ... additional arguments to be passed to \code{\link{clozeQ}} for creation of the question object
#'
#' @return returns a \code{clozeL} question container object with one question added.
#' @export
#'
#' @seealso \code{\link{clozeQ}}
add_clozeQ <- function(cl, ...) {
  cq <- clozeQ(...)
  cl[[length(cl)+1]] <- cq
  cl
}


#' print_answerlist
#' @description Print the MCQ answerlist from the clozeL object as rmarkdown into the exam. Empty lines for non-MCQ questions are included as required by the exams package.
#' @param cl clozeL container object.
#'
#' @export
#'
#' @examples
#' data('mtcars')
#' cl <- new_clozeL('Car quiz')
#' # Question: How many cylinders does a Datsun 710 have?
#' cl <- add_schoice(cl, options=sort(unique(mtcars$cyl)), correctno=1, points=1, shuffle=FALSE)
#' print_answerlist(cl)
print_answerlist <- function(cl) {
  answerlist <- function(...) {
    writeLines(c("Answerlist", "----------",paste("*", do.call("paste", list(...,sep = '. '))),'*  '))
  }
  answerlist(unlist(lapply(cl, function(x) x$mcq_options)))
}

#' extract_element
#' @description Extract one element from all questions contained in the clozeL container object.
#' @param cl clozeL question container object
#' @param elname character with the name of the element to extract from each clozeQ object (e.g. \code{points})
#'
#' @return returns a vector which collects element \code{elname} from all \code{clozeQ} contained in \code{clozeL}.
#'
extract_element <- function(cl, elname) {
  unlist(lapply(cl, function(x) x[[elname]]))
}

#' print_metainfo
#' @description Collect and print as markdown all meta-information from a clozeL object.
#' @param cl clozeL question container object
#'
#' @export
#'
#' @examples
#' data('mtcars')
#' cl <- new_clozeL('Car quiz')
#' # Question: How many cylinders does a Datsun 710 have?
#' cl <- add_schoice(cl, options=sort(unique(mtcars$cyl)), correctno=1, points=1, shuffle=FALSE)
#' print_metainfo(cl)
print_metainfo <- function(cl) {
  writeLines(c("Meta-information", "================",
               paste0("exname: ",attr(cl, 'Qname')),
               paste0("extype: ", 'cloze'),
               paste0("exsolution: ", paste0(extract_element(cl,'answer'),collapse='|')),
               paste0("exclozetype: ", paste0(extract_element(cl,'type'),collapse='|')),
               paste0("expoints: ", paste0(extract_element(cl,'points'),collapse='|')),
               paste0("extol: ", paste0(extract_element(cl,'tol'),collapse='|')),
               paste0("exextra[exptfrac]: ", paste0(extract_element(cl,'pointfrac'),collapse='|')),
               paste0("exextra[exmchoice_display]: ", paste0(extract_element(cl,'mchoice_display'),collapse='|'))
  ))
}

#' stdround
#' @description Standard "school" rounding as opposed to the rounding of the \code{round} function.
#' @param x number to round
#' @param digits number of decimal places
#'
#' @return returns rounded number (numeric).
#' @export
#'
#' @examples stdround(4.255,2)
stdround <- function(x,digits=3){
  pmsign <- sign(x)
  z <- trunc(abs(x)*10^digits + 0.5)
  z <- z/10^digits
  return(z*pmsign)
}

#' write_regression_equation
#' @description Write an estimated regression equation from a \code{lm} regression object as LaTeX string.
#' @param reg regression object.
#' @param digits number of digits to round to.
#'
#' @return Latex string.
#' @export
#' @import stats
#'
#' @examples
#' data('mtcars')
#' m <- lm(mpg ~ disp + wt, data=mtcars)
#' write_regression_equation(m)
write_regression_equation <- function(reg, digits=3) {
  cfs <- stdround(coefficients(reg), digits=digits)
  paste0('\\widehat{', names(attr(reg$terms, 'dataClasses')[1]), '}=',
         paste0(paste0(cfs[1], paste0(cfs[-1], '\\times ', names(cfs[-1]), collapse='')),
                collapse=ifelse(cfs[-1]>=0,'+','')), collapse='')
}


