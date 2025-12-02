priority_narrative <- function(df) {
  df2 <- df %>% mutate(Priority_Number_num = readr::parse_number(as.character(Priority_Number)))
  freq <- df2 %>% count(Priority_Number_num) %>% arrange(desc(n))
  if (nrow(freq) == 0) return("No priority data available.")
  top <- freq[1,]
  paste0(
    "The majority of calls were Priority ", top$Priority_Number_num,
    ", representing ", round(100 * top$n / nrow(df2), 1), "% of calls."
  )
}
