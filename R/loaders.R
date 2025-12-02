suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
})

load_week_data <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  readr::read_csv(path, show_col_types = FALSE)
}

load_phone_data <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  readr::read_csv(path, show_col_types = FALSE)
}
