##
##      __________  _______________       _________  ______________  __   _  ________  ___   ___    __    __
##     / ____/ __ \/_  __/_  __/   |     / ____/   |/_  __/ ____/ / / /  ( )/ ____/  |/  /  /   |  / /   / /
##    / / __/ / / / / /   / / / /| |    / /   / /| | / / / /   / /_/ /   |// __/ / /|_/ /  / /| | / /   / /
##   / /_/ / /_/ / / /   / / / ___ |   / /___/ ___ |/ / / /___/ __  /     / /___/ /  / /  / ___ |/ /___/ /___
##   \____/\____/ /_/   /_/ /_/  |_|   \____/_/  |_/_/  \____/_/ /_/     /_____/_/  /_/  /_/  |_/_____/_____/
##   

# Package crawler's side-kicks =================================================

# Reverse a string
str_rev <- function(str) { 
  
  library(purrr)
  
  if (!length(str)) return(character(0))
  strsplit(str, NULL) %>% 
    map(., ~ rev(.x) %>% paste(., collapse = "")) %>% 
    unlist()
}

# DIR DIVER
dir_diver <- function(root, pattern = ".R$") {
  list.files(root, pattern = pattern, full.names = TRUE, recursive = TRUE)
}

# PACKAGE PARSER
pkg_parser <- function(file) {
  
  library(magrittr)
  
  if (!file.exists(file)) stop("File doesn't exist!")
  
  # Scan file
  lines <- scan(file, what = character(), sep = "\n")
  lines <- trimws(lines)
  
  # Extract packages
  easy <- lines %>%
    grep("(require\\()|(library\\()", ., val = TRUE) %>%
    gsub(".*library\\((.*)\\).*", "\\1", .) %>%
    gsub(".*require\\((.*)\\).*", "\\1", .)
  diff <- grep("\\:\\:", lines, val = TRUE) %>% 
    gsub("\\:\\:.*$", "", .) %>% 
    str_rev(.) %>% 
    gsub("([a-zA-z0-9._]+).*", "\\1", .) %>% 
    str_rev(.)
  
  unique(c(easy, diff))
}

# Main Function ================================================================

##
# PACKAGE CRAWLER
#
# Returns all packages that were used in the R scripts that lie in root
# Input: root, the directory name that contains R scripts and possibly other 
#        directories
# Output: a vector with all package names found
#
pkg_crawler <- function(root) {
  # Go, DIRECTORY DIVER!
  dirs <- dir_diver(root)
  if (!length(dirs)) stop("No R files found to parse")
  # Finish 'em, PACKAGE PARSER!
  pkgs <- lapply(dirs, pkg_parser)
  pkgs <- unique(unlist(pkgs))
  return(pkgs)
}
  