suppressPackageStartupMessages({
  library(readr)
})

parse_priority_number <- function(x) {
  readr::parse_number(as.character(x))
}

palette_cyan_base <- c("#E0F2F1", "#B2DFDB", "#80CBC4", "#4DB6AC", "#26A69A", "#009688", "#00897B", "#00796B", "#00695C", "#004D40")
palette_blue_base <- c("#08306B", "#08519C", "#2171B5", "#4292C6", "#6BAED6", "#9ECAE1", "#C6DBEF", "#DEEBF7")
palette_red_base  <- c("#67000D", "#A50F15", "#CB181D", "#EF3B2C", "#FB6A4A", "#FC9272", "#FEE0D2")
