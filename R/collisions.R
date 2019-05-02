#' Check for Namespace Collisions
#'
#' Check for namespace collisions with functions and packages on CRAN
#' @name CRAN_collisions
#'
#' @usage CRAN_collisions(function_or_package_name)
#'
#' @param function_or_package_name A character string, or vector of character strings.
#'
#' @import dplyr
#' @import stringr
#' @importFrom utils data
#'
#' @export
#'
#' @examples
#'
#'# Test single function name
#'function_or_package_name <- "a3.r2"
#'CRAN_collisions(function_or_package_name)
#'
#'# Test multiple function names
#'function_or_package_name <- c("a3.r2", "xtable.A3")
#'CRAN_collisions(function_or_package_name)
#'
#'# Test single package name
#'function_or_package_name <- "dplyr"
#'CRAN_collisions(function_or_package_name)
#'
#'# Test multiple package names
#'function_or_package_name <- c("dplyr", "data.frame", "gsubfn")
#'CRAN_collisions(function_or_package_name)
#'
#'
#'
library(dplyr)
library(stringr)



CRAN_collisions <- function(function_or_package_name) {
  packages_and_functions_dataframe <- NULL
  # if(missing(check_packages)) { check_packages <- TRUE }
  # if(missing(check_functions)) { check_functions <- TRUE }
  # cran_packages <- get(load("data/packages_and_functions_dataframe.RData"))
  data(packages_and_functions_dataframe, envir = environment())
  cran_packages <- packages_and_functions_dataframe
  if(missing(function_or_package_name)) { stop("Please provide a function or package name to check") }


  # Returns whether or not a package name collision as occurred

  package_collisions <- which(cran_packages$package_names %in% function_or_package_name) %>%
    cran_packages$package_names[.] %>% unique(.)


  # Returns data.frame of function name collisions
  function_collisions <- which(cran_packages$function_names %in% function_or_package_name) %>%
    cran_packages[., ]


  output_list <- list("packages"= NULL, "functions"= NULL)

  output_list$packages <- package_collisions
  output_list$functions <- function_collisions

  output_list %>% return

}






#' Check for Namespace Collisions
#'
#' Check for namespace collisions with packages on CRAN
#' @name CRAN_package_collisions
#'
#' @usage CRAN_package_collisions(package_name)
#'
#' @param package_name A character string, or vector of character strings.
#'
#' @import dplyr
#' @import stringr
#' @importFrom utils data
#'
#' @export
#'
#' @examples
#'
#'
#'# Test single package name
#'package_name <- "dplyr"
#'CRAN_package_collisions(package_name)
#'
#'# Test multiple package names
#'package_name <- c("dplyr", "data.frame", "gsubfn")
#'CRAN_package_collisions(package_name)
#'
#'
#'
library(dplyr)
library(stringr)



CRAN_package_collisions <- function(package_name) {
  packages_and_functions_dataframe <- NULL
  # if(missing(check_packages)) { check_packages <- TRUE }
  # if(missing(check_functions)) { check_functions <- TRUE }
  data(packages_and_functions_dataframe, envir = environment())
  cran_packages <- packages_and_functions_dataframe
  if(missing(package_name)) { stop("Please provide a function or package name to check") }


  # Returns whether or not a package name collision as occurred

  package_collisions <- which(cran_packages$package_names %in% package_name) %>%
    cran_packages$package_names[.] %>% unique(.)


  output_list <- list("packages"= NULL)

  output_list$packages <- package_collisions

  output_list %>% return

}











#' Check for Namespace Collisions
#'
#' Check for namespace collisions with functions on CRAN
#' @name CRAN_function_collisions
#'
#' @usage CRAN_function_collisions(function_name)
#'
#' @param function_name A character string, or vector of character strings.
#'
#' @import dplyr
#' @import stringr
#' @importFrom utils data
#'
#' @export
#'
#' @examples
#'
#'# Test single function name
#'function_name <- "a3.r2"
#'CRAN_function_collisions(function_name)
#'
#'# Test multiple function names
#'function_name <- c("a3.r2", "xtable.A3")
#'CRAN_function_collisions(function_name)
#'
#'
#'
library(dplyr)
library(stringr)



CRAN_function_collisions <- function(function_name) {
  packages_and_functions_dataframe <- NULL
  # if(missing(check_packages)) { check_packages <- TRUE }
  # if(missing(check_functions)) { check_functions <- TRUE }
  data(packages_and_functions_dataframe, envir = environment())
  cran_packages <- packages_and_functions_dataframe
  if(missing(function_name)) { stop("Please provide a function or package name to check") }


  # Returns whether or not a package name collision as occurred

  package_collisions <- which(cran_packages$package_names %in% function_name) %>%
    cran_packages$package_names[.] %>% unique(.)


  # Returns data.frame of function name collisions
  function_collisions <- which(cran_packages$function_names %in% function_name) %>%
    cran_packages[., ]


  output_list <- list("functions"= NULL)

  output_list$functions <- function_collisions

  output_list %>% return

}








#' Show the packages and functions that are on CRAN
#'
#' Retrieve functions from CRAN.
#' @name CRAN_packages_and_functions
#'
#' @usage CRAN_packages_and_functions()
#'
#' @import dplyr
#' @importFrom utils data
#'
#' @export
#'
#' @examples
#'
#' # Retrieve CRAN packages and functions
#' CRAN_packages_and_functions()
#'
#'




CRAN_packages_and_functions <- function() {
  packages_and_functions_dataframe <- NULL
  data(packages_and_functions_dataframe, envir = environment())
  cran_packages <- packages_and_functions_dataframe
  cran_packages
}

CRAN_packages_and_functions()









#' Show the packages that are on CRAN
#'
#' Retrieve a list of packages on CRAN.
#' @name CRAN_packages
#'
#' @usage CRAN_packages()
#'
#' @import dplyr
#' @importFrom utils data
#'
#' @export
#'
#' @examples
#'
#' # Retrieve CRAN packages
#' CRAN_packages()
#'
#'




CRAN_packages <- function() {
  packages_and_functions_dataframe <- NULL
  data(packages_and_functions_dataframe, envir = environment())
  cran_packages <- packages_and_functions_dataframe
  cran_packages$package_names %>% unique
}

CRAN_packages()







#' Show the functions that are on CRAN
#'
#' Retrieve functions from CRAN.
#' @name CRAN_functions
#'
#' @usage CRAN_functions()
#' @importFrom utils data
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#'
#' # Retrieve CRAN functions
#' CRAN_functions()
#'
#'




CRAN_functions <- function() {
  packages_and_functions_dataframe <- NULL
  data(packages_and_functions_dataframe, envir = environment())
  cran_packages <- packages_and_functions_dataframe
  cran_packages$function_names %>% unique
}

CRAN_functions()















