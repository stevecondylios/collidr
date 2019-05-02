
#' Check for namespace collisions
#'
#' Check for namespace collisions with functions and packages on CRAN.
#' @name collisions
#'
#' @usage collisions(function_or_package_name)
#'
#' @param function_or_package_name A character string, or vector of character strings.
#'
#' @import dplyr
#' @import stringr
#' @import data.table
#'
#' @export
#'
#' @examples
#'
#'# Test single function name
#'function_or_package_name <- "a3.r2"
#'collisions(function_or_package_name)
#'
#'# Test multiple function names
#'function_or_package_name <- c("a3.r2", "xtable.A3")
#'collisions(function_or_package_name)
#'
#'# Test single package name
#'function_or_package_name <- "dplyr"
#'collisions(function_or_package_name)
#'
#'# Test multiple package names
#'function_or_package_name <- c("dplyr", "data.frame", "gsubfn")
#'collisions(function_or_package_name)
#'
#'
#'
library(dplyr)
library(data.table)
library(stringr)

# Test single function name
function_or_package_name <- "a3.r2"
collisions(function_or_package_name)

# Test multiple function names
function_or_package_name <- c("a3.r2", "xtable.A3")
collisions(function_or_package_name)

# Test single package name
function_or_package_name <- "dplyr"
collisions(function_or_package_name)

# Test multiple package names
function_or_package_name <- c("dplyr", "data.frame", "gsubfn")
collisions(function_or_package_name)


collisions <- function(function_or_package_name) {

  # if(missing(check_packages)) { check_packages <- TRUE }
  # if(missing(check_functions)) { check_functions <- TRUE }
  cran_packages <- readRDS("data/packages_and_functions_dataframe.rds")
  if(missing(function_or_package_name)) { stop("Please provide a function or package name to check") }


  # Returns whether or not a package name collision as occurred

  package_collisions <- which(packages_and_functions_dataframe$package_names %in% function_or_package_name) %>%
    packages_and_functions_dataframe$package_names[.] %>% unique(.)


  # Returns data.frame of function name collisions
  function_collisions <- which(packages_and_functions_dataframe$function_names %in% function_or_package_name) %>%
    packages_and_functions_dataframe[., ]


  output_list <- list("packages"= NULL, "functions"= NULL)

  output_list$packages <- package_collisions
  output_list$functions <- function_collisions

  output_list %>% return

}





#' Show packages on CRAN
#'
#' Check for namespace collisions with functions and packages on CRAN.
#' @name CRAN_packages
#'
#' @usage CRAN_packages()
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#'
#' # Provide a salary string and 'extract_salary' and will extract the salary and return it
#' CRAN_packages()
#'
#'




CRAN_packages <- function() {
  cran_packages <- readRDS("data/packages_and_functions_dataframe.rds")
  cran_packages
}








