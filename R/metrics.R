suppressPackageStartupMessages({
  library(dplyr)
})

compute_phone_week_metrics <- function(phone_df) {
  if (is.null(phone_df) || nrow(phone_df) == 0) return(NULL)
  list(
    pct15 = mean(phone_df$`911_PCT_15`, na.rm = TRUE) * 100,
    pct20 = mean(phone_df$`911_PCT_20`, na.rm = TRUE) * 100
  )
}
