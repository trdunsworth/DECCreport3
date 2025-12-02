suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
})

priority_bar <- function(df, palette = NULL) {
  df <- df %>% mutate(
    Priority_Number_num = readr::parse_number(as.character(Priority_Number)),
    Priority_Number_fac = factor(
      Priority_Number_num,
      levels = sort(unique(na.omit(Priority_Number_num))),
      ordered = TRUE
    )
  )
  ggplot(df, aes(x = Priority_Number_fac, fill = Priority_Number_fac)) +
    geom_bar() +
    scale_fill_manual(values = if (is.null(palette)) palette_cyan_base else palette) +
    labs(x = "Priority Level", y = "Number of Calls") +
    geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 3.5) +
    theme_minimal() +
    theme(legend.position = "none")
}
