#' collidr: Check for Namespace Collisions with Functions and Packages on CRAN
#' @docType package
#'
#' @details
#' It has the goal of providing a convenient and native way of checking for namespace collisions when writing functions and packages.
#'
#'
#' @author Steve Condylios \email{steve.condylios@gmail.com}
#'
#'
#'
#' @name collidr-package
#'
# NULL
# From jennybc's comment here: https://github.com/STAT545-UBC/Discussion/issues/451
# quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
