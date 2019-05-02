
library(rvest)
library(pdftools)
library(dplyr)
library(data.table)
library(stringr)


# Get package names and links from CRAN

cran_splash <- read_html("https://cran.r-project.org/web/packages/available_packages_by_name.html")

packages <- cran_splash %>% html_nodes("table a") %>% html_attr("href") %>% strsplit(., "/") %>%
  sapply(., function(x) { x[5] } )

links <- cran_splash %>% html_nodes("table a") %>% html_attr("href") %>% substr(., 6, nchar(.)) %>%
  paste0("https://cran.r-project.org", .)

reference_manual_links <- packages %>% paste0("https://cran.r-project.org/web/packages/", . , "/", ., ".pdf")
if(!dir.exists("temp")) { system("mkdir temp") }

packages_and_functions_dataframe <- data.frame("package_name"=as.character(), "function_name"=as.character())
total_number_of_functions <- 0

missing_package_urls <- c()


# saveRDS(packages_and_functions_dataframe, "packages_and_functions_dataframe_first_1670.rds")
# packages_and_functions_dataframe <- readRDS("packages_and_functions_dataframe_first_1670.rds")
for (i in 4511:length(packages)) {

  
  # i <- 1
  
  
  print(paste0("Scraping package ", i, " of ", length(packages), " - ", packages[i], 
               " (", total_number_of_functions, " functions collected so far)"))
  
  
  skip_to_next <<- FALSE
  tryCatch(manual_pdf <-  reference_manual_links[i] %>% pdf_text, 
           error = function(e) {
             missing_package_urls <<- c(missing_package_urls, reference_manual_links[i])
             print(paste0("Skipped package ", packages[i]))
             skip_to_next <<- TRUE
           }
  )
  
  if(skip_to_next) { next } 
  
  # manual_pdf %>% paste0(., collapse="")
  
  # Note
  # a <- list(c("a","b"), c("c", "d"))
  # a %>% unlist %>% paste0(., collapse="")
  # [1] "abcd"
  
  one_manual_pdf <- manual_pdf %>% unlist %>% paste0(., collapse="")
  
  temp <- one_manual_pdf %>% str_split("R topics documented:")
  
  
  # Logic: After strsplit, discard the first item, and unlist 2nd and subsequent items 
  # (this will fail if the term "R topics documented:" happens to appear on the first page of the manual)
  # Then, discard everying after the term 'Index'
  
  length_temp <- temp[[1]] %>% length
  relevant_temp <- temp[[1]] %>% .[(2):(length_temp)] %>% unlist %>% paste0(., collapse="")
  
  # Note the number of spaces after "\nIndex" is sufficiently short to allow for tens of thousands
  # pages in the manual, which presumably won't ever happen 
  
  relevant_temp <- relevant_temp %>% 
    strsplit(., "\nIndex                                                                      ",
             fixed = TRUE) %>%
    .[[1]] %>% .[1]
  
  
  # Logic: strsplit every space, then discard any ".", then discard any strings containing \n
  # grepl("a", c("abc", "def")) # [1]  TRUE FALSE
  
  # inspect
  relevant_temp %>% strsplit(. , " ")
  
  
  # The step after this will leave the very last page number given in the index, since it doesn't
  # have a line break after it. So we remove it first
  # "abc" %>% str_remove(., "b") # [1] "ac"
  
  # Known imperfection: this will only remove the page number form the last page of the index,
  # not every page. The result is that for packages with more than one index page, the last page
  # number on the page will slip through for every page other than the last
  
  relevant_temp <- relevant_temp %>% str_remove(., "(\\d+)$")
  
  function_names <- relevant_temp %>% strsplit(. , " ") %>% unlist %>% {.[!grepl("\n", .)] } %>% 
  { .[!(nchar(.) == 1 | nchar(.) == 0)] }
  
  total_number_of_functions <- total_number_of_functions + length(function_names)
  
  package_names <- rep(packages[i], length(function_names))
  
  output <- data.frame(package_names, function_names, stringsAsFactors = FALSE)
  
  packages_and_functions_dataframe <- rbind(packages_and_functions_dataframe, output)

}




# saveRDS(packages_and_functions_dataframe, "packages_and_functions_dataframe.RDS")
# saveRDS(missing_package_urls, "missing_package_urls.RDS")
# 
# missing_package_urls
# packages_and_functions_dataframe <- readRDS("packages_and_functions_dataframe.RDS")
# 






collisions <- function(function_or_package_name) {
  
  # if(missing(check_packages)) { check_packages <- TRUE }
  # if(missing(check_functions)) { check_functions <- TRUE }
  if(missing(function_or_package_name)) { stop("Please provide a function or package name to check") }

  # Test single function name
  function_or_package_name <- "a3.r2"
  
  # Test multiple function names
  function_or_package_name <- c("a3.r2", "xtable.A3")
  
  # Returns data.frame of function name collisions
  function_collisions <- which(packages_and_functions_dataframe$function_names %in% function_or_package_name) %>% 
    packages_and_functions_dataframe[., ]
  
  if(function_collisions %>% nrow %>% {. > 0 }) { function_collisions_present <- TRUE} else { 
    function_collisions_present <- FALSE}
  
  # Returns whether or not a package name collision as occurred
  package_collision_present <- packages_and_functions_dataframe$package_names %>% unique %>% 
    { . %in% function_or_package_name } %>% sum %>% {. > 0}
  
  output_list <- list("packages"=c(), "functions"=c())
  
  if(package_collision_present)
  
  
  
  
}






which(unique_functions %in% function_or_package_name)
which(function_or_package_name %in% unique_functions)


which("b" %in% c("a", "b", "c"))

which(c("a", "b", "c") %in% "b")




# Possible namespace collision with <package>
# Please check manual: www.linktomanual.com















































# Function to discard any lines (or portions of lines) of NAMESPACE that are simply comments
disard_comments <- function(NAMESPACE_line) {
  
  # Test
  # NAMESPACE_line <- c("export(abc, cv4abc, postpr, cv4postpr, expected.deviance, gfit, gfitpca)",
  #                     "# abc S3methods", "S3method(print, abc)", "# cv4abc S3methods",
  #                     "S3method(summary, postpr)", "test # string")
  
  
  NAMESPACE_line %>% strsplit(., "#") %>% sapply(., function(x) { x[1]})
  
}







# Get namespace file
namespace_links <- packages[1:10] %>% paste0("https://cran.r-project.org/web/packages/", ., "/NAMESPACE")




















get_functions <- function(package) {

  package <- "abc"  # Remove this when done

  namespace_link <- package %>% paste0("https://cran.r-project.org/web/packages/", ., "/NAMESPACE")

  lines_of_NAMESPACE <- namespace_link %>% read_html(.) %>% html_nodes("body") %>% html_text %>% strsplit("\n")

  relevant_lines_of_NAMESPACE <- lines_of_NAMESPACE %>% sapply(. , function(x) { x %like% "S3|^export[^exportPattern]" } ) %>%
    .[,1] %>% lines_of_NAMESPACE[[1]][.] %>% disard_comments %>% .[. != ""] %>% { .[. %like% "export"] }


  # Two possibilities:
  # export(function_name)
  # export(package_name, function_name) # package <- "abc"


  comma_counts <- relevant_lines_of_NAMESPACE %>% sapply(., function(x) {str_count(x, ",")} )

  relevant_lines_of_NAMESPACE %>% .[comma_counts >= 2] %>% strsplit(., "\\(") %>% .[[1]] %>% .[2] %>%
    strsplit(., "\\)") %>% .[[1]] %>% .[1] %>% strsplit(., ",") %>% sapply(., trimws)









}







# We can see from here: http://r-pkgs.had.co.nz/namespace.html
# That of the 8 namespace directives, 4 describe exports

# exportPattern() accepts a pattern, meaning it's not useful here
# And to generate a comprehensive list of functions, those
# exported using exportPattern would need to be included somehow







# Tests
example1 <- "# Generated by roxygen2: do not edit by hand\nexport(extractr)"

example1 %>% strsplit(., "\n")

# Where abc is the package name
example2 <- "# Generated by roxygen2: do not edit by hand

S3method(summary, abc)"





