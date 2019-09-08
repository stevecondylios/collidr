#' Check for Namespace Collisions
#'
#' Check for namespace collisions with functions and packages on CRAN
#' @name CRAN_collisions
#'
#' @usage CRAN_collisions(function_or_package_name, CRANdf)
#'
#' @param function_or_package_name A character string, or vector of character strings.
#' @param CRANdf Optionally provide an updated CRAN data.frame (obtain with getCRAN())
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



CRAN_collisions <- function(function_or_package_name, CRANdf) {

  if(missing(CRANdf)) { CRANdf <- get("CRANdf", pos = "package:collidr") }

  cran_packages <- CRANdf
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
#' @usage CRAN_package_collisions(package_name, CRANdf)
#'
#' @param package_name A character string, or vector of character strings.
#' @param CRANdf Optionally provide an updated CRAN data.frame (obtain with getCRAN())
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

CRAN_package_collisions <- function(package_name, CRANdf) {
  if(missing(CRANdf)) { CRANdf <- get("CRANdf", pos = "package:collidr") }

  cran_packages <- CRANdf
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
#' @usage CRAN_function_collisions(function_name, CRANdf)
#'
#' @param function_name A character string, or vector of character strings.
#' @param CRANdf Optionally provide an updated CRAN data.frame (obtain with getCRAN())
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



CRAN_function_collisions <- function(function_name, CRANdf) {

  if(missing(CRANdf)) { CRANdf <- get("CRANdf", pos = "package:collidr") }

  cran_packages <- CRANdf
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
#' Show functions from CRAN.
#' @name CRAN_packages_and_functions
#'
#' @usage CRAN_packages_and_functions(CRANdf)
#'
#' @param CRANdf Optionally provide an updated CRAN data.frame (obtain with getCRAN())
#'
#' @import dplyr
#' @importFrom utils data
#'
#' @export
#'
#' @examples
#'
#' # Show CRAN packages and functions
#' CRAN_packages_and_functions()
#'
#'




CRAN_packages_and_functions <- function(CRANdf) {

  if(missing(CRANdf)) { CRANdf <- get("CRANdf", pos = "package:collidr") }

  cran_packages <- CRANdf
  cran_packages
}


#' @rdname CRAN_packages_and_functions
#' @export
CRANpf <- CRAN_packages_and_functions









#' Show the packages that are on CRAN
#'
#' Show a list of packages on CRAN.
#' @name CRAN_packages
#'
#' @usage CRAN_packages(CRANdf)
#'
#' @param CRANdf Optionally provide an updated CRAN data.frame (obtain with getCRAN())
#'
#' @import dplyr
#' @importFrom utils data
#'
#' @export
#'
#' @examples
#'
#' # Show CRAN packages
#' CRAN_packages()
#'
#'



CRAN_packages <- function(CRANdf) {

  if(missing(CRANdf)) { CRANdf <- get("CRANdf", pos = "package:collidr") }

  cran_packages <- CRANdf
  cran_packages$package_names %>% unique
}


#' @rdname CRAN_packages
#' @export
CRANp <- CRAN_packages







#' Show the functions that are on CRAN
#'
#' Show functions from CRAN.
#' @name CRAN_functions
#'
#' @usage CRAN_functions(CRANdf)
#'
#' @param CRANdf Optionally provide an updated CRAN data.frame (obtain with getCRAN())
#'
#' @importFrom utils data
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#'
#' # Show CRAN functions
#' CRAN_functions()
#'
#'




CRAN_functions <- function(CRANdf) {

  if(missing(CRANdf)) { CRANdf <- get("CRANdf", pos = "package:collidr") }

  cran_packages <- CRANdf
  cran_packages$function_names %>% unique
}



#' @rdname CRAN_functions
#' @export
CRANf <- CRAN_functions









#' Retrieve a more up to date data.frame of packages and functions from CRAN
#'
#' Retrieve a more up to date data.frame of packages and functions from CRAN
#' @name getCRAN
#'
#' @usage getCRAN()
#' @importFrom utils data
#'
#' @import dplyr jsonlite
#' @import jsonlite
#' @export
#'
#' @examples
#'\dontrun{
#' # Retrieve CRAN functions
#' CRAN_updated <- getCRAN()
#'}
#'
#'
#'
getCRAN <- function() {

  print("Retrieving CRAN data..")

  s3url <- "https://collidr-api.s3-ap-southeast-2.amazonaws.com/pfd.RDS"

  pfd <- tryCatch(readRDS(url(s3url)),
                  error=function(e) { stop("Something went wrong
                                           - please try again later") })
  pfd <- pfd[!duplicated(pfd),]
  pfd <- pfd %>% arrange(.data$package_names, .data$function_names)
  pfd[] <- pfd %>% lapply(as.character)

    last_updated <- s3url %>% curlGetHeaders %>% .[5] %>% trimws %>%
      strsplit(., "Last-Modified: ") %>%
      .[[1]] %>% .[2] %>% strsplit(., " ") %>%
      {.[[1]][-c(1,6)]} %>% paste0(., collapse=" ") %>%
      as.POSIXct(., format="%d %b %Y %H:%M:%OS")
    attr(last_updated, "tzone") <- "UTC"
    attr(pfd, "last_updated") <- last_updated
    print(paste0("Data last updated ", last_updated, " UTC"))

  return(pfd)
}







