#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: setup
#| echo: false
#| message: false
#| warning: false

# Suppress dbus warnings that can cause preview issues
suppressWarnings({
  suppressPackageStartupMessages({
    library(tidyverse)
    library(tidymodels)
    library(devtools)
    library(remotes)
    library(ggpubr)
    library(ggrepel)
    library(ggraph)
    library(gt)
    library(gtExtras)
    library(GGally)
    library(rstatix)
    library(car)
    library(janitor)
    library(Hmisc)
    library(psych)
    library(corrr)
    library(ggcorrplot)
    library(ggthemes)
    library(ggridges)
    library(multcomp)
    library(emmeans)
    library(RVAideMemoire)
    library(FactoMineR)
    library(DescTools)
    library(nlme)
    library(funModeling)
    library(inspectdf)
    library(dlookr)
    library(viridis)
    library(merTools)
    library(factoextra)
    library(nortest)
    library(MASS)
    library(randtests)
    library(summarytools)
    library(report)
    library(knitr)
    library(kableExtra)
    library(modelbased)
    library(parameters)
    library(performance)
    library(insight)
    library(lubridate)
    library(broom)
    library(GPfit)
    library(survival)
    library(paletteer)
    library(flextable)
    library(officer)
  })
})

# Verify paletteer is available
#if (!requireNamespace("paletteer", quietly = TRUE)) {
#  stop("paletteer package is not installed or available")
#} else {
#  cat("✓ paletteer package loaded successfully\n")
#}

# Define week number for dynamic title generation
WEEK_NUMBER <- 39

# Calculate week dates properly for 2025
# Week 38 should be Sept 14-20, 2025 (Sunday to Saturday) - CORRECTED
# Sunday-first week calculation

# Method 1: Direct date specification for week 38
if(WEEK_NUMBER == 39) {
  WEEK_START_DATE <- as.Date("2025-09-21")  # Sunday Sept 14 (CORRECTED)
  WEEK_END_DATE <- as.Date("2025-09-27")    # Saturday Sept 20 (CORRECTED)
} else {
  # For other weeks, we can use a more general calculation
  # Find the first Sunday of 2025 (which is actually Dec 29, 2024)
  jan_1_2025 <- as.Date("2025-01-01")
  # January 1, 2025 was a Wednesday
  # First Sunday is the Sunday before or on Jan 1
  first_sunday_2025 <- jan_1_2025 - days(wday(jan_1_2025) - 1)  # Dec 29, 2024
  
  # Calculate the start date for the specified week
  WEEK_START_DATE <- first_sunday_2025 + weeks(WEEK_NUMBER - 1)
  WEEK_END_DATE <- WEEK_START_DATE + days(6)
}

# Format dates for display
WEEK_START_FORMATTED <- format(WEEK_START_DATE, "%d %b")
WEEK_END_FORMATTED <- format(WEEK_END_DATE, "%d %b")

# Update title dynamically (since we can't use R in YAML)
# Note: You'll need to manually update the YAML title, or use this for reference
DYNAMIC_TITLE <- paste0("Weekly Report: Week ", WEEK_NUMBER, " (", WEEK_START_FORMATTED, " through ", WEEK_END_FORMATTED, ")")

# Print the calculated dates for verification
# cat("Week", WEEK_NUMBER, "dates:", WEEK_START_FORMATTED, "through", WEEK_END_FORMATTED, "\n")
#
#
#
#| echo: false
#| message: false
#| warning: false

flextable::set_flextable_defaults(
  theme_fun = flextable::theme_booktabs,
  font.size = 11,
  padding = 3,
  borders = officer::fp_border_default()
)

to_ft <- function(tbl, caption = NULL, header_map = NULL, digits = 2) {
  df <- as.data.frame(tbl)
  ft <- flextable::flextable(df)
  if (!is.null(header_map)) ft <- flextable::set_header_labels(ft, values = header_map)
  if (nrow(df) > 0) ft <- flextable::bg(ft, i = seq(1, nrow(df), by = 2), bg = "#F5F5F5", part = "body")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::fontsize(ft, part = "header", size = 12)
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  if (length(num_cols) > 0) {
    ft <- flextable::align(ft, j = num_cols, align = "right", part = "all")
    ft <- flextable::colformat_num(ft, j = num_cols, digits = digits, big.mark = ",")
  }
  if (!is.null(caption)) ft <- flextable::set_caption(ft, caption)
  flextable::autofit(ft)
}
#
#
#
#
#
#
#
#| echo: false
#| output: false

# Dynamic data file loading with error handling
# Check for different naming conventions
possible_files <- c(
  paste0("data/week", WEEK_NUMBER, ".csv"),
  paste0("data/Week", WEEK_NUMBER, "_25.csv"),
  paste0("data/Week", WEEK_NUMBER, ".csv")
)

# Find the first file that exists
data_file <- NULL
for(file in possible_files) {
  if(file.exists(file)) {
    data_file <- file
    break
  }
}

if(is.null(data_file)) {
  stop("No data file found for week ", WEEK_NUMBER, ". Checked: ", paste(possible_files, collapse = ", "))
}

cat("Loading data from:", data_file, "\n")

# Use read_csv with error handling and show parsing issues
df <- read_csv(data_file, show_col_types = FALSE)

# Check if data loaded correctly and show any parsing problems
if(nrow(df) == 0) {
  stop("Data file is empty or failed to load: ", data_file)
}

# Report any parsing problems
parsing_problems <- problems(df)
if(nrow(parsing_problems) > 0) {
  cat("Warning: Found", nrow(parsing_problems), "parsing issues in data\n")
}

cat("Successfully loaded", nrow(df), "rows from", data_file, "\n")
cat("Call_Reception column check:", "Call_Reception" %in% names(df), "\n")
 
# Use lubridate and across() to efficiently parse all date-time columns
df <- df |>
  mutate(across(c(Response_Date,
                   Incident_Start_Time,
                   TimeCallViewed,
                   Incident_Queue_Time,
                   Incident_Dispatch_Time,
                   Incident_Phone_Stop,
                   TimeFirstUnitDispatchAcknowledged,
                   Incident_Enroute_Time,
                   Incident_Arrival_Time,
                   TimeFirstCallCleared,
                   Incident_First_Close_Time,
                   Final_Closed_Time,
                   First_Reopen_Time), ymd_hms))

df$WeekNo <- as.factor(df$WeekNo)
df$Day <- as.factor(df$Day)
df$Hour <- as.factor(df$Hour)

# Convert DOW to an ordered factor to respect the sequence of days
df$DOW <- factor(
    df$DOW,
    levels = c("SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT"),
    ordered = TRUE
)

df$ShiftPart <- factor(
  df$ShiftPart,
  levels = c("EARLY", "MIDS", "LATE"),
  ordered = TRUE
)

# Convert Priority_Number to an ordered factor as well
df$Priority_Number <- ordered(df$Priority_Number)

# Convert numeric variables from 'doubles' to integers
df[c('Time_To_Queue', 'Time_To_Dispatch', 'Phone_Time', 'Processing_Time', 'Rollout_Time', 'Transit_Time', 'Total_Call_Time')] <- sapply(df[c('Time_To_Queue', 'Time_To_Dispatch', 'Phone_Time', 'Processing_Time', 'Rollout_Time', 'Transit_Time', 'Total_Call_Time')], as.numeric)
#
#
#
#
#
#| echo: false
#| tbl-cap: "A sample of the first 10 rows of incident data."

colnames(df)
#
#
#
#
#
#
#
#| echo: false
#| fig-cap: "Prevalence of missing values. Only columns with missing data are shown."

# ...existing code...
# Count columns with any missing data
missing_cols_count <- sum(colSums(is.na(df)) > 0)

# Get named vector of missing counts per column
missing_counts <- colSums(is.na(df))

# Optionally, create a tibble for easy use
missing_summary <- tibble(
  column = names(df),
  missing = missing_counts
)

# Find the column with the largest number of missing values
max_missing_col <- names(missing_counts)[which.max(missing_counts)]
max_missing_count <- max(missing_counts)

# Calculate percentage of calls without Incident_Arrival_Time
incident_arrival_missing_pct <- round(missing_counts["Incident_Arrival_Time"] / nrow(df) * 100, 1)

# Identify columns with missing data for inline reporting
incident_arrival_missing_count <- missing_counts["Incident_Arrival_Time"]

# ...existing code...

inspect_na(df) |>
  dplyr::filter(cnt > 0) |> # Explicitly use dplyr's filter
  show_plot(
    text_labels = TRUE,
    label_color = "white"
  ) +
  paletteer::scale_fill_paletteer_d("MexBrewer::Maiz") +
  geom_text(
    aes(label = paste0(round(pcnt, 1), "%")),
    stat = "identity",
    vjust = -0.5,
    size = 5,
    color = "black",
    fontface = "bold"
  ) +
  ggthemes::theme_fivethirtyeight(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11), # Rotate x-axis labels
    axis.text.y = element_text(size = 12) # Fine-tune y-axis label size
  )
#
#
#
#
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by day of the week."
# ggplot2
dow_counts <- df |>
  count(DOW, sort = TRUE)

max_dow_info <- dow_counts |> filter(n == max(n))
busiest_day_abbr <- max_dow_info |> slice(1) |> pull(DOW)
busiest_day_count <- max_dow_info |> slice(1) |> pull(n)

min_dow_info <- dow_counts |> filter(n == min(n))
slowest_day_abbr <- min_dow_info |> slice(1) |> pull(DOW)
slowest_day_count <- min_dow_info |> slice(1) |> pull(n)

# Create mapping from abbreviations to full day names
day_names <- c(
  "SUN" = "Sunday",
  "MON" = "Monday",
  "TUE" = "Tuesday",
  "WED" = "Wednesday",
  "THU" = "Thursday",
  "FRI" = "Friday",
  "SAT" = "Saturday"
)

# Convert abbreviations to full day names
busiest_day <- day_names[busiest_day_abbr]
slowest_day <- day_names[slowest_day_abbr]

barDOW <- df |> ggplot(aes(x=DOW, fill=DOW)) +
  geom_bar() +
  paletteer::scale_fill_paletteer_d("ggsci::cyan_material") +
  labs(title="Number of Calls for Service by Day of the Week",
       x="Day of the Week",
       y="Number of Calls") +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size=6
  ) +
  theme_minimal() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size=18),
        axis.text.x = element_text(angle=45, hjust=1, size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

barDOW
#
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by hour of the day."
# ggplot2
hour_counts <- df |>
  count(Hour, sort = TRUE)

# Find the actual maximum count and corresponding hour(s)
max_count <- max(hour_counts$n)
max_hour_info <- hour_counts |> filter(n == max_count)

# Convert factor levels back to numeric for proper comparison, then back to formatted string
busiest_hour_numeric <- max_hour_info |> 
  slice(1) |> 
  pull(Hour) |> 
  as.character() |> 
  as.numeric()
busiest_hour <- sprintf("%02d", busiest_hour_numeric)
busiest_hour_count <- max_hour_info |> slice(1) |> pull(n)

min_hour_info <- hour_counts |> filter(n == min(n))
# Convert factor levels back to numeric for proper comparison, then back to formatted string  
slowest_hour_numeric <- min_hour_info |> 
  slice(1) |> 
  pull(Hour) |> 
  as.character() |> 
  as.numeric()
slowest_hour <- sprintf("%02d", slowest_hour_numeric)
slowest_hour_count <- min_hour_info |> slice(1) |> pull(n)

barHour <- df |> ggplot(aes(x=Hour, fill=Hour)) +
  geom_bar() +
  # Use ggsci::cyan_material palette scaled to 24 levels for hours
  {
    cyan_material_cols <- tryCatch(
      {
        as.character(paletteer::paletteer_d("ggsci::cyan_material"))
      },
      error = function(e) {
        # Fallback cyan colors if palette not available
        c("#E0F2F1", "#B2DFDB", "#80CBC4", "#4DB6AC", "#26A69A", "#009688", "#00897B", "#00796B", "#00695C", "#004D40")
      }
    )
    scale_fill_manual(values = grDevices::colorRampPalette(cyan_material_cols)(24))
  } +
  labs(title="Number of Calls for Service by Hour of the Day",
       x="Hour of the Day",
       y="Number of Calls") +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size=6
  ) +
  theme_minimal() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size=18),
        axis.text.x = element_text(angle=45, hjust=1, size=12),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

barHour
#
#
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by shift."
shift_counts <- df |>
  count(Shift, sort = TRUE)

dn_counts <- df |>
  count(Day_Night, sort = TRUE)

sp_counts <- df |>
  count(ShiftPart, sort = TRUE) # Note: ShiftPart is not used in the plots below

barShift <- df |>
  ggplot(aes(x=Shift, fill=ShiftPart)) +
  geom_bar() +
  paletteer::scale_fill_paletteer_d("ggsci::cyan_material") +
  labs(title="Call Volume per Shift",
    x = "Shift",
    y = "Number of Calls") +
  # Add segment counts centered within each segment (white text)
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_stack(vjust = 0.5),
    size=6,
    color = "black",
    fontface = "bold"
  ) +
  # Add total counts at the top of each bar
  stat_count(
    aes(label = after_stat(count), fill = NULL),
    geom = "text",
  vjust = -0.5,
    size=6.5,
    fontface = "bold",
    color = "black"
  ) +
  theme_minimal() +
  theme(legend.position="right",
        plot.title = element_text(hjust = 0.5, size=18),
        axis.text.x = element_text(angle=45, hjust=1, size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12))

barShift

barDN <- df |>
  ggplot(aes(x=Day_Night, fill=ShiftPart)) +
  geom_bar() +
  paletteer::scale_fill_paletteer_d("ggsci::cyan_material") +
  labs(title="Call Volume by Day/Night",
       x="Day/Night",
       y="Number of Calls") +
  # Add segment counts centered within each segment (white text)
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_stack(vjust = 0.5),
    size=6,
    color = "black",
    fontface = "bold"
  ) +
  # Add total counts at the top of each bar
  stat_count(
    aes(label = after_stat(count), fill = NULL),
    geom = "text",
  vjust = -0.5,
    size=6.5,
    fontface = "bold",
    color = "black"
  ) +
  theme_minimal() +
  theme(legend.position="right",
        plot.title = element_text(hjust = 0.5, size=18),
        axis.text.x = element_text(angle=45, hjust=1, size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12))

barDN
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by priority level."
# ggplot2
pn_counts <- df |>
  count(Priority_Number, sort = TRUE)

max_pn_info <- pn_counts |> filter(n == max(n))
busiest_pn <- max_pn_info |> slice(1) |> pull(Priority_Number)
busiest_pn_count <- max_pn_info |> slice(1) |> pull(n)

# Calculate percentage statistics for inline use
busiest_pn_percentage <- round(busiest_pn_count / nrow(df) * 100, 1)
priority1_count <- sum(df$Priority_Number == "1", na.rm = TRUE)
priority1_percentage <- round(priority1_count / nrow(df) * 100, 1)

barPriority <- df |> ggplot(aes(x=Priority_Number, fill=Priority_Number)) +
  geom_bar() +
  paletteer::scale_fill_paletteer_d("ggsci::cyan_material") +
  labs(title="Number of Calls for Service by Priority Level",
       x="Priority Level",
       y="Number of Calls") +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size=6
  ) + 
  theme_minimal() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size=18),
        axis.text.x = element_text(angle=45, hjust=1, size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

barPriority
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by discipline."
# ggplot2
agency_counts <- df |>
  count(Agency, sort = TRUE)

max_agency_info <- agency_counts |> filter(n == max(n))
busiest_agency <- max_agency_info |> slice(1) |> pull(Agency)
busiest_agency_count <- max_agency_info |> slice(1) |> pull(n)

# Calculate percentage for Police calls
police_percentage <- round((sum(df$Agency == "POLICE", na.rm = TRUE) / nrow(df)) * 100, 1)

barDiscipline <- df |> ggplot(aes(x=Agency, fill=Agency)) +
  geom_bar() + 
  scale_fill_manual(
    values = c(POLICE = "#1f77b4", FIRE = "#d62728", EMS = "#2ca02c"),
    name = "Agency"
  ) +
  labs(title="Number of Calls for Service by Discipline",
       x="Discipline",
       y="Number of Calls") +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_stack(vjust = 0.5),
    size=6,
    color = "white",
    fontface = "bold"
  ) + 
  theme_minimal() +
  theme(legend.position="right",
        plot.title = element_text(hjust = 0.5, size=18),
        axis.text.x = element_text(angle=45, hjust=1, size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12))

barDiscipline
#
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by call reception."

# ggplot2
cr_counts <- df |>
  count(Call_Reception, sort = TRUE)

max_cr_info <- cr_counts |> filter(n == max(n))
busiest_cr <- max_cr_info |> slice(1) |> pull(Call_Reception)
busiest_cr_count <- max_cr_info |> slice(1) |> pull(n)

barReception <- df |> ggplot(aes(x=Call_Reception, fill=Call_Reception)) +
  geom_bar() + 
  paletteer::scale_fill_paletteer_d("ggsci::cyan_material") +
  labs(title="Number of Calls for Service by Call Reception",
       x="Call Reception",
       y="Number of Calls") +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size=6
  ) + 
  theme_minimal() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size=18),
        axis.text.x = element_text(angle=45, hjust=1, size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

barReception
#
#
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by call type."
# ggplot2
ct_counts <- df |>
  count(Problem, sort = TRUE)

max_ct_info <- ct_counts |> filter(n == max(n))
busiest_ct <- max_ct_info |> slice(1) |> pull(Problem)
busiest_ct_count <- max_ct_info |> slice(1) |> pull(n)

# Find the most common Problem for each Agency
agency_top_problems <- df |>
  count(Agency, Problem, sort = TRUE) |>
  group_by(Agency) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  dplyr::select(Agency, Problem, n)

# Create named vectors for easy access in inline code
agency_top_problem_names <- setNames(agency_top_problems$Problem, agency_top_problems$Agency)
agency_top_problem_counts <- setNames(agency_top_problems$n, agency_top_problems$Agency)

problem_counts <- df |>
  count(Problem, sort = TRUE) |>
  slice_head(n = 10)

barProblem <- problem_counts |>
  ggplot(aes(x=reorder(Problem, -n), y=n, fill=Problem)) +
  geom_bar(stat="identity") +
  paletteer::scale_fill_paletteer_d("ggsci::cyan_material") +
  labs(title="Number of Calls for Service by Call Type",
       x="Call Type",
       y="Number of Calls") +
  geom_text(
    aes(label = n),
    vjust = -0.5, 
    size = 6) +
  theme_minimal() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size=18),
        axis.text.x = element_text(angle=45, hjust=1, size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

barProblem
#
#
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by telecommunicator."
# ggplot2
tc_counts <- df |>
  count(Call_Taker, sort = TRUE)

max_tc_info <- tc_counts |> filter(n == max(n))
busiest_tc <- max_tc_info |> slice(1) |> pull(Call_Taker)
busiest_tc_count <- max_tc_info |> slice(1) |> pull(n)

tc_counts <- df |> 
  count(Call_Taker, sort = TRUE) |>
  slice_head(n = 10)

barCallTaker <- tc_counts |>
  ggplot(aes(x=reorder(Call_Taker, -n), y=n, fill=Call_Taker)) +
  geom_bar(stat="identity") +
  paletteer::scale_fill_paletteer_d("ggsci::cyan_material") +
  labs(title="Number of Calls for Service by Call Taker",
       x="Call Taker",
       y="Number of Calls") +
  geom_text(
    aes(label = n),
    vjust = -0.5,
    size = 6) +
  theme_minimal() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size=18),
        axis.text.x = element_text(angle=45, hjust=1, size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

barCallTaker
#
#
#
#
#
#| label: time-on-phone
#| echo: false
#| message: false
#| warning: false
#| fig-cap: "Total Time on Phone for Top 10 calltakers"

# Two tables: top 10 Call_Taker by cumulative Time_To_Queue and Phone_Time

# Ensure numeric seconds for aggregation
df_times <- df |>
  mutate(
    Time_To_Queue = suppressWarnings(as.numeric(Time_To_Queue)),
    Phone_Time    = suppressWarnings(as.numeric(Phone_Time))
  )

# Summarize cumulative seconds by Call_Taker
ttq_by_ct <- df_times |>
  filter(!is.na(Call_Taker)) |>
  group_by(Call_Taker) |>
  dplyr::summarise(
    N_TTQ = sum(!is.na(Time_To_Queue)),
    Total_Time_To_Queue_Sec = sum(Time_To_Queue, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(Total_Time_To_Queue_Sec)) |>
  slice_head(n = 10) |>
  mutate(
    Total_Time_To_Queue_Min = round(Total_Time_To_Queue_Sec / 60, 1),
    Mean_TTQ_Sec = ifelse(N_TTQ > 0, round(Total_Time_To_Queue_Sec / N_TTQ), NA_real_)
  )

  most_time_ct <- ttq_by_ct$Call_Taker[1]
  most_time_ct_total <- ttq_by_ct$Total_Time_To_Queue_Sec[1]
  most_time_ct_mean <- ttq_by_ct$Mean_TTQ_Sec[1]

phone_by_ct <- df_times |>
  filter(!is.na(Call_Taker)) |>
  group_by(Call_Taker) |>
  dplyr::summarise(
    N_Phone = sum(!is.na(Phone_Time)),
    Total_Phone_Time_Sec = sum(Phone_Time, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(Total_Phone_Time_Sec)) |>
  slice_head(n = 10) |>
  mutate(
    Total_Phone_Time_Min = round(Total_Phone_Time_Sec / 60, 1),
    Mean_Phone_Sec = ifelse(N_Phone > 0, round(Total_Phone_Time_Sec / N_Phone), NA_real_)
  )

  longest_to_queue_ct <- phone_by_ct$Call_Taker[1]
  longest_to_queue_ct_total <- phone_by_ct$Total_Phone_Time_Sec[1]
  longest_to_queue_ct_mean <- phone_by_ct$Mean_Phone_Sec[1]

# Render tables with accessible Word-friendly formatting
to_ft(
  ttq_by_ct |> dplyr::select(Call_Taker, N_TTQ, Total_Time_To_Queue_Sec, Total_Time_To_Queue_Min, Mean_TTQ_Sec),
  caption = "Top 10 Call Takers by Cumulative Time To Queue",
  header_map = list(
    Call_Taker = "Call Taker",
    N_TTQ = "Calls",
    Total_Time_To_Queue_Sec = "Total TTQ (sec)",
    Total_Time_To_Queue_Min = "Total TTQ (min)",
    Mean_TTQ_Sec = "Mean TTQ (sec)"
  ),
  digits = 0
)

to_ft(
  phone_by_ct |> dplyr::select(Call_Taker, N_Phone, Total_Phone_Time_Sec, Total_Phone_Time_Min, Mean_Phone_Sec),
  caption = "Top 10 Call Takers by Cumulative Phone Time",
  header_map = list(
    Call_Taker = "Call Taker",
    N_Phone = "Calls",
    Total_Phone_Time_Sec = "Total Phone (sec)",
    Total_Phone_Time_Min = "Total Phone (min)",
    Mean_Phone_Sec = "Mean Phone (sec)"
  ),
  digits = 0
)
#
#
#
#
#
#
#
#
#
#| label: hour-dow-analysis
#| echo: false
#| message: false
#| warning: false
#| fig-cap: "Call Volume by Hour and Day of Week"

# Create summary data
hourly_dow_summary <- df |>
  group_by(DOW, Hour) |>
  summarise(call_count = n(), .groups = 'drop') |>
  mutate(Hour_numeric = as.numeric(as.character(Hour)))

# Create a heatmap showing call patterns
hour_dow_plot <- ggplot(hourly_dow_summary, aes(x = Hour_numeric, y = DOW, fill = call_count)) +
  geom_tile(color = "white", linewidth = 0.1) +
  scale_x_continuous(name = "Hour of Day",
                     breaks = seq(0, 23, 2),
                     labels = sprintf("%02d:00", seq(0, 23, 2))) +
  scale_y_discrete(name = "Day of Week", limits = rev) +
  scale_fill_gradient2(name = "Calls",
                       low = "lightblue",
                       mid = "yellow",
                       high = "red",
                       midpoint = median(hourly_dow_summary$call_count)) +
  labs(title = "Call Volume Heatmap by Hour and Day of Week",
       subtitle = "Darker colors indicate higher call volumes") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.grid = element_blank()
  )

hour_dow_plot
#
#
#
#| label: alternative-ridge-plot
#| echo: false
#| message: false
#| warning: false
#| fig-cap: "Ridge Plot Alternative - Calls per Hour by Day of Week"

# Try to create actual ridge plot if ggridges is available
tryCatch({
  if (requireNamespace("ggridges", quietly = TRUE)) {
    library(ggridges)
    
    # Create ridge plot using the raw data for a density estimate
    ridge_plot <- ggplot(df, aes(x = as.numeric(as.character(Hour)), y = DOW, fill = DOW)) +
      ggridges::geom_density_ridges(
  alpha = 0.7,
        scale = 1.2, # Adjust scale for better separation
        rel_min_height = 0.01 # Removes trailing tails
      ) +
      scale_x_continuous(name = "Hour of Day",
                         breaks = seq(0, 23, 4),
                         labels = sprintf("%02d:00", seq(0, 23, 4))) +
      scale_y_discrete(name = "Day of Week", limits = rev) +
      scale_fill_brewer(name = "Day", palette = "Set3") +
      labs(title = "Ridge Plot: Call Volume Distribution by Hour and Day of Week",
           subtitle = "Each ridge shows the hourly distribution for one day") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.position = "none"
      )

    print(ridge_plot)
  } else {
    cat("ggridges package not available. Using heatmap above instead.\n")
  }
}, error = function(e) {
  cat("Could not create ridge plot. Error:", e$message, "\n")
  cat("The heatmap above provides similar insights.\n")
})
#
#
#
#| label: ridge-plot-summary-stats
#| echo: false
#| message: false
#| warning: false

# Summary statistics for calls by hour and DOW
hourly_summary <- df |>
  group_by(DOW) |>
  summarise(
    total_calls = n(),
    peak_hour = names(sort(table(Hour), decreasing = TRUE))[1],
    avg_calls_per_hour = round(n() / 24, 1),
    .groups = 'drop'
  ) |>
  arrange(desc(total_calls))

# Display summary table
to_ft(
  hourly_summary,
  caption = "Call Volume Summary by Day of Week — Peak hours and average calls per hour",
  header_map = list(
    DOW = "Day of Week",
    total_calls = "Total Calls",
    peak_hour = "Peak Hour",
    avg_calls_per_hour = "Avg Calls/Hour"
  ),
  digits = 0
)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: custom-summary
#| echo: false
#| message: false
#| warning: false

# Initialize variables
summary_table <- NULL
median_time_to_queue <- NA_real_
median_time_to_dispatch <- NA_real_

# Create a summary table of elapsed time variables
tryCatch({
  # Define the columns we want to analyze
  time_columns <- c("Time_To_Queue", "Time_To_Dispatch", "Phone_Time",
                    "Processing_Time", "Rollout_Time", "Transit_Time", "Total_Call_Time")

  # Check which columns actually exist in the data
  existing_columns <- time_columns[time_columns %in% names(df)]

  if (length(existing_columns) == 0) {
    cat("No time columns found in the data.\n")
  } else {

    summary_table <- df %>%
      # 1. Select only the columns that exist
      dplyr::select(all_of(existing_columns)) %>%
      # 2. Summarize across all selected columns, converting difftime to numeric safely
      summarise(across(everything(),
        list(
          Minimum  = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(min(vals, na.rm = TRUE), 2)
          },
          Mean     = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(mean(vals, na.rm = TRUE), 2)
          },
          Median   = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(median(vals, na.rm = TRUE), 2)
          },
          Std_Dev  = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(sd(vals, na.rm = TRUE), 2)
          },
          Skewness = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else {
              tryCatch(round(psych::skew(vals, na.rm = TRUE), 2), error = function(e) NA_real_)
            }
          },
          Kurtosis = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals)) || length(vals[!is.na(vals)]) < 4) {
              NA_real_
            } else {
              tryCatch({
                # Remove NA values first
                clean_vals <- vals[!is.na(vals)]

                # Try multiple approaches for kurtosis calculation
                result <- tryCatch({
                  # Method 1: Use psych::kurtosis with explicit namespace
                  psych::kurtosis(clean_vals)
                }, error = function(e1) {
                  tryCatch({
                    # Method 2: Use moments package if available
                    if (requireNamespace("moments", quietly = TRUE)) {
                      moments::kurtosis(clean_vals) - 3  # Convert to excess kurtosis
                    } else {
                      stop("moments not available")
                    }
                  }, error = function(e2) {
                    # Method 3: Manual calculation
                    n <- length(clean_vals)
                    if (n < 4) return(NA_real_)

                    mean_val <- mean(clean_vals)
                    var_val <- var(clean_vals)

                    if (var_val == 0) return(NA_real_)

                    # Calculate fourth moment
                    fourth_moment <- mean((clean_vals - mean_val)^4)
                    # Calculate kurtosis (excess kurtosis = kurtosis - 3)
                    kurt_val <- (fourth_moment / (var_val^2)) - 3
                    kurt_val
                  })
                })

                round(result, 2)
              }, error = function(e) {
                # Final fallback
                NA_real_
              })
            }
          }
        ),
        .names = "{.col}---{.fn}" # Use a unique separator
      )) %>%
      # 3. Reshape the data to a long format, then back to a clean wide format
      pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
      separate(Variable, into = c("Variable", "Statistic"), sep = "---") %>%
      pivot_wider(names_from = Statistic, values_from = Value) %>%
      mutate(Variable = str_replace_all(Variable, "_", " ")) # Clean up names for display

    # Extract key metrics for use in text
  median_time_to_queue <- summary_table |>
  filter(Variable == "Time To Queue") |>
      pull(Median)
    median_time_to_queue_ca <- median_time_to_queue
    median_time_to_queue_ems <- median_time_to_queue
    median_time_to_queue_afd <- median_time_to_queue
    # Preserve overall median for reuse later
    median_time_to_queue_overall <- median_time_to_queue

  median_time_to_dispatch <- summary_table |>
  filter(Variable == "Time To Dispatch") |>
      pull(Median)
    median_time_to_dispatch_ca <- median_time_to_dispatch
    median_time_to_dispatch_ems <- median_time_to_dispatch
    median_time_to_dispatch_afd <- median_time_to_dispatch
    # Preserve overall median for reuse later
    median_time_to_dispatch_overall <- median_time_to_dispatch

    median_processing_time <- summary_table |>
      filter(Variable == "Processing_Time") |>
      pull(Median)
    median_processing_time_ca <- median_processing_time
    median_processing_time_ems <- median_processing_time
    median_processing_time_afd <- median_processing_time
    # Preserve overall median for reuse later
    median_processing_time_overall <- median_processing_time

    median_phone_time <- summary_table |>
      filter(Variable == "Phone_Time") |>
      pull(Median)
    median_phone_time_ca <- median_phone_time
    median_phone_time_ems <- median_phone_time
    median_phone_time_afd <- median_phone_time
    # Preserve overall median for reuse later
    median_phone_time_overall <- median_phone_time

    to_ft(
      summary_table,
      caption = "Weekly Elapsed Time Summary Table — Statistical summary of call processing times",
      header_map = list(
        Variable = "Time Metric",
        Minimum = "Min",
        Mean = "Mean",
        Median = "Median",
        Std_Dev = "Std Dev",
        Skewness = "Skew",
        Kurtosis = "Kurt"
      ),
      digits = 2
    )
  }
}, error = function(e) {
  cat("Error creating summary table:", e$message, "\n")
  cat("Available columns in df:", paste(names(df), collapse = ", "), "\n")
})
#
#
#
#
#
#| label: ttq-plots
#| echo: false
#| message: false
#| warning: false

# Helper to format seconds as mm:ss or h:mm:ss
sec_label <- function(x) {
  s <- round(x)
  h <- s %/% 3600
  m <- (s %% 3600) %/% 60
  sec <- s %% 60
  ifelse(h > 0, sprintf("%d:%02d:%02d", h, m, sec), sprintf("%d:%02d", m, sec))
}

# Prepare TTQ values in seconds and basic stats
ttq <- df |>
  transmute(ttq_sec = as.numeric(Time_To_Queue)) |>
  filter(!is.na(ttq_sec), ttq_sec >= 0)

n_ttq <- nrow(ttq)
ttq_med <- median(ttq$ttq_sec, na.rm = TRUE)
ttq_p90 <- as.numeric(quantile(ttq$ttq_sec, 0.90, na.rm = TRUE))
ttq_p99 <- as.numeric(quantile(ttq$ttq_sec, 0.99, na.rm = TRUE))
n_clipped <- sum(ttq$ttq_sec > ttq_p99, na.rm = TRUE)

# Adaptive bin width (Freedman–Diaconis) with nicening
fd <- 2 * IQR(ttq$ttq_sec, na.rm = TRUE) / (n_ttq^(1/3))
binw <- fd
if (!is.finite(binw) || binw <= 0) binw <- 5
binw <- dplyr::case_when(
  binw < 1 ~ 1,
  binw < 2 ~ 2,
  binw < 5 ~ 5,
  binw < 10 ~ 10,
  binw < 15 ~ 15,
  TRUE ~ round(binw, -1)
)

# Clip extreme tail for readability (ensure at least 20s to show reference lines)
x_max <- max(20, ttq_p99)

# Single plot: histogram (normalized to density) with density overlay and markers
ttq_hist_dens <- ggplot(ttq, aes(x = ttq_sec)) +
  # Histogram scaled to density for alignment with density curve
  geom_histogram(aes(y = after_stat(density)),
                 binwidth = binw, boundary = 0, closed = "left",
                 fill = "#1c5789", color = "white", alpha = 0.6) +
  # Density overlay
  geom_density(color = "#1c5789", fill = "#1c5789", alpha = 0.2, adjust = 1.1, linewidth = 1) +
  # Median and P90 reference lines
  geom_vline(xintercept = ttq_med, linetype = "dashed", color = "#d62728", linewidth = 0.8) +
  geom_vline(xintercept = ttq_p90, linetype = "dotted", color = "#ff7f0e", linewidth = 0.8) +
  # NENA/NFPA reference lines at 0:15 and 0:20
  geom_vline(xintercept = 15, linetype = "longdash", color = "#2ca02c", linewidth = 0.7) +
  geom_vline(xintercept = 20, linetype = "longdash", color = "#9467bd", linewidth = 0.7) +
  # Labels for markers (placed above the density peak area)
  annotate("label", x = ttq_med, y = Inf, vjust = 1.2,
           label = paste0("Median: ", round(ttq_med), "s"), size = 4, fill = "white") +
  annotate("label", x = ttq_p90, y = Inf, vjust = 1.2,
           label = paste0("P90: ", round(ttq_p90), "s"), size = 4, fill = "white") +
  annotate("label", x = 15, y = Inf, vjust = 1.2,
           label = "NENA 0:15", size = 4, fill = "white") +
  annotate("label", x = 20, y = Inf, vjust = 1.2,
           label = "NFPA 0:20", size = 4, fill = "white") +
  scale_x_continuous(name = "Time to Queue (mm:ss)",
                     limits = c(0, x_max),
                     breaks = scales::pretty_breaks(8),
                     labels = sec_label) +
  scale_y_continuous(name = "Density") +
  labs(title = "Time to Queue — Histogram with Density",
    subtitle = paste0("Histogram normalized to density; clipped at 99th percentile (", scales::comma(n_clipped), " removed).\n",
          "Median (dashed red), 90th percentile (dotted orange). Reference lines at 0:15 (NENA) and 0:20 (NFPA 1225)."),
       caption = "Data: CAD") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

ttq_hist_dens

#
#
#
#
#
#| label: elapsed-time-plots
#| echo: false
#| message: false
#| warning: false

# Reusable plot helper: histogram normalized to density with density overlay and markers
plot_time_hist_dens <- function(data, var, title_text) {
  vals <- as.numeric(data[[var]])
  vals <- vals[is.finite(vals) & vals >= 0]
  if (length(vals) < 2) return(NULL)

  # Stats
  n <- length(vals)
  med <- median(vals)
  p90 <- as.numeric(quantile(vals, 0.90))
  p99 <- as.numeric(quantile(vals, 0.99))
  n_clipped <- sum(vals > p99)

  # Freedman–Diaconis bin width with nicening
  fd <- 2 * IQR(vals) / (n^(1/3))
  binw <- fd
  if (!is.finite(binw) || binw <= 0) binw <- 5
  binw <- dplyr::case_when(
    binw < 1 ~ 1,
    binw < 2 ~ 2,
    binw < 5 ~ 5,
    binw < 10 ~ 10,
    binw < 15 ~ 15,
    TRUE ~ round(binw, -1)
  )

  x_max <- max(20, p99)

  # mm:ss (or h:mm:ss) labels
  sec_label <- function(x) {
    s <- round(x)
    h <- s %/% 3600
    m <- (s %% 3600) %/% 60
    sec <- s %% 60
    ifelse(h > 0, sprintf("%d:%02d:%02d", h, m, sec), sprintf("%d:%02d", m, sec))
  }

  ggplot(data.frame(x = vals), aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)),
                   binwidth = binw, boundary = 0, closed = "left",
                   fill = "#1c5789", color = "white", alpha = 0.6) +
    geom_density(color = "#1c5789", fill = "#1c5789", alpha = 0.2, adjust = 1.1, linewidth = 1) +
    geom_vline(xintercept = med, linetype = "dashed", color = "#d62728", linewidth = 0.8) +
    geom_vline(xintercept = p90, linetype = "dotted", color = "#ff7f0e", linewidth = 0.8) +
    geom_vline(xintercept = 15, linetype = "longdash", color = "#2ca02c", linewidth = 0.7) +
    geom_vline(xintercept = 20, linetype = "longdash", color = "#9467bd", linewidth = 0.7) +
  annotate("label", x = med, y = Inf, vjust = 1.2,
       label = "Median", size = 3.5, fill = "white") +
  annotate("label", x = p90, y = Inf, vjust = 1.2,
       label = "P90", size = 3.5, fill = "white") +
  annotate("label", x = 15, y = Inf, vjust = 1.2,
       label = "NENA", size = 3.5, fill = "white") +
  annotate("label", x = 20, y = Inf, vjust = 1.2,
       label = "NFPA", size = 3.5, fill = "white") +
    scale_x_continuous(name = "Time (mm:ss)",
                       limits = c(0, x_max),
                       breaks = scales::pretty_breaks(8),
                       labels = sec_label) +
    scale_y_continuous(name = "Density") +
    labs(title = title_text,
      subtitle = NULL,
      caption = "Data: CAD") +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    )
}

# Build and print plots for other elapsed-time metrics
p_dispatch   <- plot_time_hist_dens(df, "Time_To_Dispatch",  "Time to Dispatch — Histogram with Density")
p_phone      <- plot_time_hist_dens(df, "Phone_Time",       "Phone Time — Histogram with Density")
p_processing <- plot_time_hist_dens(df, "Processing_Time",  "Processing Time — Histogram with Density")
p_rollout    <- plot_time_hist_dens(df, "Rollout_Time",     "Rollout Time — Histogram with Density")
p_transit    <- plot_time_hist_dens(df, "Transit_Time",     "Transit Time — Histogram with Density")
p_total      <- plot_time_hist_dens(df, "Total_Call_Time",  "Total Call Time — Histogram with Density")

if (!is.null(p_dispatch))   print(p_dispatch)
if (!is.null(p_phone))      print(p_phone)
if (!is.null(p_processing)) print(p_processing)
if (!is.null(p_rollout))    print(p_rollout)
if (!is.null(p_transit))    print(p_transit)
if (!is.null(p_total))      print(p_total)
#
#
#
#| label: elapsed-time-grid
#| echo: false
#| message: false
#| warning: false

# Short-title versions for grid
p_dispatch_grid   <- plot_time_hist_dens(df, "Time_To_Dispatch",  "Time to Dispatch")
p_phone_grid      <- plot_time_hist_dens(df, "Phone_Time",       "Phone Time")
p_processing_grid <- plot_time_hist_dens(df, "Processing_Time",  "Processing Time")
p_rollout_grid    <- plot_time_hist_dens(df, "Rollout_Time",     "Rollout Time")
p_transit_grid    <- plot_time_hist_dens(df, "Transit_Time",     "Transit Time")
p_total_grid      <- plot_time_hist_dens(df, "Total_Call_Time",  "Total Call Time")

# Arrange elapsed-time plots in a 2x3 grid (skip NULL plots)
plots_list <- list(
  p_dispatch_grid,  # Time To Dispatch
  p_phone_grid,     # Phone Time
  p_processing_grid,# Processing Time
  p_rollout_grid,   # Rollout Time
  p_transit_grid,   # Transit Time
  p_total_grid      # Total Call Time
)
plots_list <- Filter(Negate(is.null), plots_list)

if (length(plots_list) > 0) {
  grid <- ggpubr::ggarrange(plotlist = plots_list, ncol = 3, nrow = 2, align = "hv")
  print(grid)
}
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: new-datasets
#| echo: false
#| message: false
#| warning: false

df_hp <- df |>
  filter((Agency == "POLICE" & Priority_Number < 2) | (Agency %in% c("FIRE", "EMS") & Priority_Number < 3))

df_law <- df |> filter(Agency == "POLICE")
df_fire <- df |> filter(Agency == "FIRE")
df_ems <- df |> filter(Agency == "EMS")
df_ttq_delay <- df_hp |> filter(Time_To_Queue > 60)
df_ttd_delay <- df_hp |> filter(Time_To_Dispatch > 60)
df_ca <- df |> filter(Problem == "CARDIAC ARREST ALS 2- SUPV")

mental_health <- c("MUTUAL PSYCHOLOGICAL EMERGENCY", "PSYCHIATRIC EMERGENCY ALS 1", "PSYCHIATRIC EMERGENCY VIOLENT", "WELFARE CHECK", "JUMPER FROM WWB", "MENTAL HEALTH CASE", "SUICIDE DELAY", "SUICIDE IN PROG NO INJ", "SUICIDE IN PROG INJ/PILLS", "SUICIDE IN PROG TRAUMA")

# Mental health related calls subset
# - Filters rows where `Problem` is one of the values in `mental_health`
# - Uses `%in%` and handles potential NA values safely with `is.na()` check
# - Creates a new dataset `df_mh` for downstream analysis
df_mh <- df |> dplyr::filter(!is.na(Problem) & Problem %in% mental_health)

# E-911 Calls subset
# - Filters rows where `Call_Reception` is `E-911`
# - Creates a new dataset `df_911` for downstream analysis
df_911 <- df |> dplyr::filter(!is.na(Call_Reception) & Call_Reception == "E-911")
df_non_e <- df |> dplyr::filter(!is.na(Call_Reception) & Call_Reception != "E-911")

# Call Reception Not Recorded subset
# - Filters rows where `Call_Reception` was not recorded
df_nrr <- df |> dplyr::filter(!is.na(Call_Reception) & Call_Reception == "NOT RECORDED")
#
#
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by day of the week."
# ggplot2
dow_counts_law <- df_law |>
  count(DOW, sort = TRUE)

max_dow_info_law <- dow_counts_law |> filter(n == max(n))
busiest_day_abbr_law <- max_dow_info_law |> slice(1) |> pull(DOW)
busiest_day_count_law <- max_dow_info_law |> slice(1) |> pull(n)

min_dow_info_law <- dow_counts_law |> filter(n == min(n))
slowest_day_abbr_law <- min_dow_info_law |> slice(1) |> pull(DOW)
slowest_day_count_law <- min_dow_info_law |> slice(1) |> pull(n)

# Create mapping from abbreviations to full day names
day_names <- c(
  "SUN" = "Sunday",
  "MON" = "Monday",
  "TUE" = "Tuesday",
  "WED" = "Wednesday",
  "THU" = "Thursday",
  "FRI" = "Friday",
  "SAT" = "Saturday"
)

# Convert abbreviations to full day names
busiest_day_law <- day_names[busiest_day_abbr_law]
slowest_day_law <- day_names[slowest_day_abbr_law]

barDOW_APD <- df_law |> ggplot(aes(x = DOW, fill = DOW)) +
    geom_bar() +
    paletteer::scale_fill_paletteer_d("ggsci::blue_material") +
    labs(
        title = "Number of Calls for Service for APD by Day of the Week",
        x = "Day of the Week",
        y = "Number of Calls"
    ) +
    geom_text(
        stat = "count",
        aes(label = after_stat(count)),
        vjust = -0.5,
        size = 6
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
    )

barDOW_APD
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by hour of the day."
# ggplot2
hour_counts_law <- df_law |>
  count(Hour, sort = TRUE)

max_hour_info_law <- hour_counts_law |> filter(n == max(n))
busiest_hour_law <- sprintf("%02d", max_hour_info_law |> slice(1) |> pull(Hour))
busiest_hour_count_law <- max_hour_info_law |> slice(1) |> pull(n)

min_hour_info_law <- hour_counts_law |> filter(n == min(n))
slowest_hour_law <- sprintf("%02d", min_hour_info_law |> slice(1) |> pull(Hour))
slowest_hour_count_law <- min_hour_info_law |> slice(1) |> pull(n)

barHour_APD <- df_law |> ggplot(aes(x = Hour, fill = Hour)) +
    geom_bar() +
    # Build a robust APD blue palette: prefer cartography::blue.pal via paletteer, fallback to a blue ramp
    {
        apd_base_cols <- tryCatch(
            {
                as.character(paletteer::paletteer_d("cartography::blue.pal"))
            },
            error = function(e) {
                c("#08306B", "#08519C", "#2171B5", "#4292C6", "#6BAED6", "#9ECAE1", "#C6DBEF", "#DEEBF7")
            }
        )
        scale_fill_manual(values = grDevices::colorRampPalette(apd_base_cols)(nlevels(df_law$Hour)))
    } +
    labs(
        title = "Number of Calls for Service for APD by Hour of the Day",
        x = "Hour of the Day",
        y = "Number of Calls"
    ) +
    geom_text(
        stat = "count",
        aes(label = after_stat(count)),
        vjust = -0.5,
        size = 6
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
    )

barHour_APD
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by call reception."
# ggplot2
cr_counts_law <- df_law |>
  count(Call_Reception, sort = TRUE)

max_cr_info_law <- cr_counts_law |> filter(n == max(n))
busiest_cr_law <- max_cr_info_law |> slice(1) |> pull(Call_Reception)
busiest_cr_count_law <- max_cr_info_law |> slice(1) |> pull(n)

barReception_APD <- df_law |> ggplot(aes(x = Call_Reception, fill = Call_Reception)) +
    geom_bar() +
    paletteer::scale_fill_paletteer_d("ggsci::blue_material") +
    labs(
        title = "Number of Calls for Service for by Call Reception",
        x = "Call Reception",
        y = "Number of Calls"
    ) +
    geom_text(
        stat = "count",
        aes(label = after_stat(count)),
        vjust = -0.5,
        size = 6
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
    )

barReception_APD
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by call type."
# ggplot2
prob_counts_law <- df_law |>
  count(Problem, sort = TRUE)

max_prob_info_law <- prob_counts_law |> filter(n == max(n))
busiest_prob_law <- max_prob_info_law |> slice(1) |> pull(Problem)
busiest_prob_count_law <- max_prob_info_law |> slice(1) |> pull(n)

problem_counts_APD <- df_law |>
  count(Problem, sort = TRUE) |>
  slice_head(n = 10)

barProblem_APD <- problem_counts_APD |>
    ggplot(aes(x = reorder(Problem, -n), y = n, fill = Problem)) +
    geom_bar(stat = "identity") +
    paletteer::scale_fill_paletteer_d("ggsci::blue_material") +
    labs(
        title = "Number of Calls for Service by Call Type",
        x = "Call Type",
        y = "Number of Calls"
    ) +
    geom_text(
        aes(label = n),
        vjust = -0.5,
        size = 6
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
    )

barProblem_APD
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by priority level."
# ggplot2
pri_counts_law <- df_law |>
  count(Priority_Number, sort = TRUE)

max_pri_info_law <- pri_counts_law |> filter(n == max(n))
busiest_pri_law <- max_pri_info_law |> slice(1) |> pull(Priority_Number)
busiest_pri_count_law <- max_pri_info_law |> slice(1) |> pull(n)

# Calculate percentage for APD priority calls
busiest_pri_law_percentage <- round((sum(df_law$Priority_Number == busiest_pri_law, na.rm = TRUE) / nrow(df_law)) * 100, 1)

# Calculate percentage for P1 calls
p1_law_percentage <- round((sum(df_law$Priority_Number == 1, na.rm = TRUE) / nrow(df_law)) * 100, 1)

barPriority_APD <- df_law |> ggplot(aes(x = Priority_Number, fill = Priority_Number)) +
    geom_bar() +
    paletteer::scale_fill_paletteer_d("ggsci::blue_material") +
    labs(
        title = "Number of Calls for Service by Priority Level",
        x = "Priority Level",
        y = "Number of Calls"
    ) +
    geom_text(
        stat = "count",
        aes(label = after_stat(count)),
        vjust = -0.5,
        size = 6
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
    )

barPriority_APD
#
#
#
#
#
#| label: apd-custom-summary
#| echo: false
#| message: false
#| warning: false

# Initialize variables
summary_table <- NULL
median_phone_time <- NA_real_
median_processing_time <- NA_real_

# Create a summary table of elapsed time variables
tryCatch({
  # Define the columns we want to analyze
  time_columns <- c("Time_To_Queue", "Time_To_Dispatch", "Phone_Time", 
                    "Processing_Time", "Rollout_Time", "Transit_Time", "Total_Call_Time")
  
  # Check which columns actually exist in the data
  existing_columns <- time_columns[time_columns %in% names(df_law)]
  
  if (length(existing_columns) == 0) {
    cat("No time columns found in the data.\n")
  } else {
    
    summary_table <- df_law %>%
      # 1. Select only the columns that exist
      dplyr::select(all_of(existing_columns)) %>%
      # 2. Summarize across all selected columns, converting difftime to numeric safely
      summarise(across(everything(),
        list(
          Minimum  = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(min(vals, na.rm = TRUE), 2)
          },
          Mean     = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(mean(vals, na.rm = TRUE), 2)
          },
          Median   = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(median(vals, na.rm = TRUE), 2)
          },
          Std_Dev  = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(sd(vals, na.rm = TRUE), 2)
          },
          Skewness = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else {
              tryCatch(round(psych::skew(vals, na.rm = TRUE), 2), error = function(e) NA_real_)
            }
          },
          Kurtosis = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals)) || length(vals[!is.na(vals)]) < 4) {
              NA_real_
            } else {
              tryCatch({
                # Remove NA values first
                clean_vals <- vals[!is.na(vals)]
                
                # Try multiple approaches for kurtosis calculation
                result <- tryCatch({
                  # Method 1: Use psych::kurtosis with explicit namespace
                  psych::kurtosis(clean_vals)
                }, error = function(e1) {
                  tryCatch({
                    # Method 2: Use moments package if available
                    if (requireNamespace("moments", quietly = TRUE)) {
                      moments::kurtosis(clean_vals) - 3  # Convert to excess kurtosis
                    } else {
                      stop("moments not available")
                    }
                  }, error = function(e2) {
                    # Method 3: Manual calculation
                    n <- length(clean_vals)
                    if (n < 4) return(NA_real_)
                    
                    mean_val <- mean(clean_vals)
                    var_val <- var(clean_vals)
                    
                    if (var_val == 0) return(NA_real_)
                    
                    # Calculate fourth moment
                    fourth_moment <- mean((clean_vals - mean_val)^4)
                    # Calculate kurtosis (excess kurtosis = kurtosis - 3)
                    kurt_val <- (fourth_moment / (var_val^2)) - 3
                    kurt_val
                  })
                })
                
                round(result, 2)
              }, error = function(e) {
                # Final fallback
                NA_real_
              })
            }
          }
        ),
        .names = "{.col}---{.fn}" # Use a unique separator
      )) %>%
      # 3. Reshape the data to a long format, then back to a clean wide format
      pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
      separate(Variable, into = c("Variable", "Statistic"), sep = "---") %>%
      pivot_wider(names_from = Statistic, values_from = Value) %>%
      mutate(Variable = str_replace_all(Variable, "_", " ")) # Clean up names for display
    
    # Extract key metrics for use in text
    median_time_to_queue <- summary_table |> 
      filter(Variable == "Time To Queue") |> 
      pull(Median)
  # Keep a namespaced copy for APD section
  median_time_to_queue_apd <- median_time_to_queue
      
    median_time_to_dispatch <- summary_table |> 
      filter(Variable == "Time To Dispatch") |> 
      pull(Median)
  # Keep a namespaced copy for APD section
  median_time_to_dispatch_apd <- median_time_to_dispatch

    median_processing_time <- summary_table |>
      filter(Variable == "Processing_Time") |>
      pull(Median)
  # Keep a namespaced copy for APD section
  median_processing_time_apd <- median_processing_time

    median_phone_time <- summary_table |>
      filter(Variable == "Phone_Time") |>
      pull(Median)
  # Keep a namespaced copy for APD section
  median_phone_time_apd <- median_phone_time

  apd_p1_calls <- df_law |>
    dplyr::filter(Priority_Number == 1)

  apd_p1_spec <- df_law |>
    dplyr::filter(Priority_Number == 1 & Time_To_Dispatch <= 120)

  apd_p2_calls <- df_law |>
    dplyr::filter(Priority_Number == 2)

  apd_p2_spec <- df_law |>
    dplyr::filter(Priority_Number == 2 & Time_To_Dispatch <= 600)

  apd_p3_calls <- df_law |>
    dplyr::filter(Priority_Number == 3)

  apd_p3_spec <- df_law |>
    dplyr::filter(Priority_Number == 3 & Time_To_Dispatch <= 1200)

  apd_p4_calls <- df_law |>
    dplyr::filter(Priority_Number == 4)

  apd_p4_spec <- df_law |>
    dplyr::filter(Priority_Number == 4 & Time_To_Dispatch <= 3600)

  apd_p1_compliance_pct <- round((nrow(apd_p1_spec) / nrow(apd_p1_calls)) * 100, 3)
  apd_p2_compliance_pct <- round((nrow(apd_p2_spec) / nrow(apd_p2_calls)) * 100, 3)
  apd_p3_compliance_pct <- round((nrow(apd_p3_spec) / nrow(apd_p3_calls)) * 100, 3)
  apd_p4_compliance_pct <- round((nrow(apd_p4_spec) / nrow(apd_p4_calls)) * 100, 3)

    to_ft(
      summary_table,
      caption = "Weekly Elapsed Time Summary Table — Statistical summary of call processing times",
      header_map = list(
        Variable = "Time Metric",
        Minimum = "Min",
        Mean = "Mean",
        Median = "Median",
        Std_Dev = "Std Dev",
        Skewness = "Skew",
        Kurtosis = "Kurt"
      ),
      digits = 2
    )
  }
}, error = function(e) {
  cat("Error creating summary table:", e$message, "\n")
  cat("Available columns in df:", paste(names(df), collapse = ", "), "\n")
})

# Extract key metrics for inline use (outside tryCatch to ensure they're available)
median_phone_time <- tryCatch({
  if (exists("summary_table")) {
    summary_table |> filter(Variable == "Phone Time") |> pull(Median)
  } else {
    NA_real_
  }
}, error = function(e) NA_real_)

median_processing_time <- tryCatch({
  if (exists("summary_table")) {
  summary_table |> filter(Variable == "Processing Time") |> pull(Median)
  } else {
    NA_real_
  }
}, error = function(e) NA_real_)

# Calculate P4 percentage for inline use
p4_percentage_apd <- round((sum(df_law$Priority_Number == "4", na.rm = TRUE) / nrow(df_law)) * 100, 1)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by day of the week."
# ggplot2
dow_counts_fire <- df_fire |>
  count(DOW, sort = TRUE)

max_dow_info_fire <- dow_counts_fire |> filter(n == max(n))
busiest_day_abbr_fire <- max_dow_info_fire |> slice(1) |> pull(DOW)
busiest_day_count_fire <- max_dow_info_fire |> slice(1) |> pull(n)

min_dow_info_fire <- dow_counts_fire |> filter(n == min(n))
slowest_day_abbr_fire <- min_dow_info_fire |> slice(1) |> pull(DOW)
slowest_day_count_fire <- min_dow_info_fire |> slice(1) |> pull(n)

# Create mapping from abbreviations to full day names
day_names <- c(
  "SUN" = "Sunday",
  "MON" = "Monday",
  "TUE" = "Tuesday",
  "WED" = "Wednesday",
  "THU" = "Thursday",
  "FRI" = "Friday",
  "SAT" = "Saturday"
)

# Convert abbreviations to full day names
busiest_day_fire <- day_names[busiest_day_abbr_fire]
slowest_day_fire <- day_names[slowest_day_abbr_fire]

barDOW_AFD <- df_fire |> ggplot(aes(x = DOW, fill = DOW)) +
    geom_bar() +
    paletteer::scale_fill_paletteer_d("ggsci::red_material") +
    labs(
        title = "Number of Calls for Service for AFD by Day of the Week",
        x = "Day of the Week",
        y = "Number of Calls"
    ) +
    geom_text(
        stat = "count",
        aes(label = after_stat(count)),
        vjust = -0.5,
        size = 6
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
    )

barDOW_AFD
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by hour of the day."
# ggplot2
hour_counts_fire <- df_fire |>
  count(Hour, sort = TRUE)

max_hour_info_fire <- hour_counts_fire |> filter(n == max(n))
busiest_hour_fire <- sprintf("%02d", max_hour_info_fire |> slice(1) |> pull(Hour))
busiest_hour_count_fire <- max_hour_info_fire |> slice(1) |> pull(n)

min_hour_info_fire <- hour_counts_fire |> filter(n == min(n))
slowest_hour_fire <- sprintf("%02d", min_hour_info_fire |> slice(1) |> pull(Hour))
slowest_hour_count_fire <- min_hour_info_fire |> slice(1) |> pull(n)

barHour_AFD <- df_fire |> ggplot(aes(x = Hour, fill = Hour)) +
    geom_bar() +
    {
        afd_base_cols <- tryCatch({
            as.character(paletteer::paletteer_d("cartography::red.pal"))
        }, error = function(e) {
            c("#67000D", "#A50F15", "#CB181D", "#EF3B2C", "#FB6A4A", "#FC9272", "#FCBBA1", "#FEE0D2")
        })
        scale_fill_manual(values = grDevices::colorRampPalette(afd_base_cols)(nlevels(df_fire$Hour)))
    } +
    labs(
        title = "Number of Calls for Service for AFD by Hour of the Day",
        x = "Hour of the Day",
        y = "Number of Calls"
    ) +
    geom_text(
        stat = "count",
        aes(label = after_stat(count)),
        vjust = -0.5,
        size = 6
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
    )

barHour_AFD
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by call reception."
# ggplot2
cr_counts_fire <- df_fire |>
  count(Call_Reception, sort = TRUE)

max_cr_info_fire <- cr_counts_fire |> filter(n == max(n))
busiest_cr_fire <- max_cr_info_fire |> slice(1) |> pull(Call_Reception)
busiest_cr_count_fire <- max_cr_info_fire |> slice(1) |> pull(n)

busiest_cr_pct_fire <- round((sum(df_fire$Call_Reception == busiest_cr_count_fire) / nrow(df_fire)) * 100, 1)
cr_phone_pct_fire <- round((sum(df_fire$Call_Reception == "Phone") / nrow(df_fire)) * 100, 1)

barReception_AFD <- df_fire |> ggplot(aes(x = Call_Reception, fill = Call_Reception)) +
    geom_bar() +
    paletteer::scale_fill_paletteer_d("ggsci::red_material") +
    labs(
        title = "Number of Calls for Service for AFD by Call Reception",
        x = "Call Reception",
        y = "Number of Calls"
    ) +
    geom_text(
        stat = "count",
        aes(label = after_stat(count)),
        vjust = -0.5,
        size = 6
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
    )

barReception_AFD
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by call type."
# ggplot2
prob_counts_fire <- df_fire |>
  count(Problem, sort = TRUE)

max_prob_info_fire <- prob_counts_fire |> filter(n == max(n))
busiest_prob_fire <- max_prob_info_fire |> slice(1) |> pull(Problem)
busiest_prob_count_fire <- max_prob_info_fire |> slice(1) |> pull(n)

problem_counts_AFD <- df_fire |>
  count(Problem, sort = TRUE) |>
  slice_head(n = 10)

barProblem_AFD <- problem_counts_AFD |>
    ggplot(aes(x = reorder(Problem, -n), y = n, fill = Problem)) +
    geom_bar(stat = "identity") +
    paletteer::scale_fill_paletteer_d("ggsci::red_material") +
    labs(
        title = "Number of Calls for Service for AFD by Call Type",
        x = "Call Type",
        y = "Number of Calls"
    ) +
    geom_text(
        aes(label = n),
        vjust = -0.5,
        size = 6
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
    )

barProblem_AFD
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by priority level."
# ggplot2
pri_counts_fire <- df_fire |>
  count(Priority_Number, sort = TRUE)

max_pri_info_fire <- pri_counts_fire |> filter(n == max(n))
busiest_pri_fire <- max_pri_info_fire |> slice(1) |> pull(Priority_Number)
busiest_pri_count_fire <- max_pri_info_fire |> slice(1) |> pull(n)

# Calculate percentage for APD priority calls
busiest_pri_fire_percentage <- round((sum(df_fire$Priority_Number == busiest_pri_fire, na.rm = TRUE) / nrow(df_fire)) * 100, 1)

# Calculate percentage for P1 calls
p1_fire_percentage <- round((sum(df_fire$Priority_Number == 1, na.rm = TRUE) / nrow(df_fire)) * 100, 1)

barPriority_AFD <- df_fire |> ggplot(aes(x = Priority_Number, fill = Priority_Number)) +
    geom_bar() +
    paletteer::scale_fill_paletteer_d("ggsci::red_material") +
    labs(
        title = "Number of Calls for Service by Priority Level",
        x = "Priority Level",
        y = "Number of Calls"
    ) +
    geom_text(
        stat = "count",
        aes(label = after_stat(count)),
        vjust = -0.5,
        size = 6
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
    )

barPriority_AFD
#
#
#
#
#
#| label: afd-custom-summary
#| echo: false
#| message: false
#| warning: false

# Initialize variables
summary_table <- NULL
median_processing_time <- NA_real_
median_phone_time <- NA_real_
mean_phone_time <- NA_real_

# Create a summary table of elapsed time variables
tryCatch({
  # Define the columns we want to analyze
  time_columns <- c("Time_To_Queue", "Time_To_Dispatch", "Phone_Time",
                    "Processing_Time", "Rollout_Time", "Transit_Time", "Total_Call_Time")
  
  # Check which columns actually exist in the data
  existing_columns <- time_columns[time_columns %in% names(df_fire)]
  
  if (length(existing_columns) == 0) {
    cat("No time columns found in the data.\n")
  } else {
    
    summary_table <- df_fire %>%
      # 1. Select only the columns that exist
      dplyr::select(all_of(existing_columns)) %>%
      # 2. Summarize across all selected columns, converting difftime to numeric safely
      summarise(across(everything(),
        list(
          Minimum  = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(min(vals, na.rm = TRUE), 2)
          },
          Mean     = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(mean(vals, na.rm = TRUE), 2)
          },
          Median   = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(median(vals, na.rm = TRUE), 2)
          },
          Std_Dev  = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(sd(vals, na.rm = TRUE), 2)
          },
          Skewness = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else {
              tryCatch(round(psych::skew(vals, na.rm = TRUE), 2), error = function(e) NA_real_)
            }
          },
          Kurtosis = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals)) || length(vals[!is.na(vals)]) < 4) {
              NA_real_
            } else {
              tryCatch({
                # Remove NA values first
                clean_vals <- vals[!is.na(vals)]
                
                # Try multiple approaches for kurtosis calculation
                result <- tryCatch({
                  # Method 1: Use psych::kurtosis with explicit namespace
                  psych::kurtosis(clean_vals)
                }, error = function(e1) {
                  tryCatch({
                    # Method 2: Use moments package if available
                    if (requireNamespace("moments", quietly = TRUE)) {
                      moments::kurtosis(clean_vals) - 3  # Convert to excess kurtosis
                    } else {
                      stop("moments not available")
                    }
                  }, error = function(e2) {
                    # Method 3: Manual calculation
                    n <- length(clean_vals)
                    if (n < 4) return(NA_real_)
                    
                    mean_val <- mean(clean_vals)
                    var_val <- var(clean_vals)
                    
                    if (var_val == 0) return(NA_real_)
                    
                    # Calculate fourth moment
                    fourth_moment <- mean((clean_vals - mean_val)^4)
                    # Calculate kurtosis (excess kurtosis = kurtosis - 3)
                    kurt_val <- (fourth_moment / (var_val^2)) - 3
                    kurt_val
                  })
                })
                
                round(result, 2)
              }, error = function(e) {
                # Final fallback
                NA_real_
              })
            }
          }
        ),
        .names = "{.col}---{.fn}" # Use a unique separator
      )) %>%
      # 3. Reshape the data to a long format, then back to a clean wide format
      pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
      separate(Variable, into = c("Variable", "Statistic"), sep = "---") %>%
      pivot_wider(names_from = Statistic, values_from = Value) %>%
      mutate(Variable = str_replace_all(Variable, "_", " ")) # Clean up names for display
    
    # Extract key metrics for use in text
    median_time_to_queue <- summary_table |> 
      filter(Variable == "Time To Queue") |> 
      pull(Median)
      
    median_time_to_dispatch <- summary_table |> 
      filter(Variable == "Time To Dispatch") |> 
      pull(Median)
    median_time_to_dispatch_afd <- median_time_to_dispatch

    median_processing_time <- summary_table |>
      filter(Variable == "Processing_Time") |>
      pull(Median)
    median_processing_time_afd <- median_processing_time

    median_phone_time <- summary_table |>
      filter(Variable == "Phone_Time") |>
      pull(Median)
    median_phone_time_afd <- median_phone_time

    mean_phone_time <- summary_table |>
      filter(Variable == "Phone_Time") |>
      pull(Mean)
    mean_phone_time_afd <- mean_phone_time

    fire_hp_calls <- df_fire |>
      dplyr::filter(Priority_Number <= 2)

    fire_64_calls <- df_fire |>
      dplyr::filter(Priority_Number <= 2 & Time_To_Dispatch <= 64)

    fire_106_calls <- df_fire |>
      dplyr::filter(Priority_Number <= 2 & Time_To_Dispatch <= 106)

    fire_64_compliance_pct <- round((nrow(fire_64_calls) / nrow(fire_hp_calls)) * 100, 3)
    fire_106_compliance_pct <- round((nrow(fire_106_calls) / nrow(fire_hp_calls)) * 100, 3)

    to_ft(
      summary_table,
      caption = "Weekly Elapsed Time Summary Table — Statistical summary of call processing times",
      header_map = list(
        Variable = "Time Metric",
        Minimum = "Min",
        Mean = "Mean",
        Median = "Median",
        Std_Dev = "Std Dev",
        Skewness = "Skew",
        Kurtosis = "Kurt"
      ),
      digits = 2
    )
  }
}, error = function(e) {
  cat("Error creating summary table:", e$message, "\n")
  cat("Available columns in df:", paste(names(df), collapse = ", "), "\n")
})
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by day of the week."
# ggplot2
dow_counts_ems <- df_ems |>
  count(DOW, sort = TRUE)

max_dow_info_ems <- dow_counts_ems |> filter(n == max(n))
busiest_day_abbr_ems <- max_dow_info_ems |> slice(1) |> pull(DOW)
busiest_day_count_ems <- max_dow_info_ems |> slice(1) |> pull(n)

min_dow_info_ems <- dow_counts_ems |> filter(n == min(n))
slowest_day_abbr_ems <- min_dow_info_ems |> slice(1) |> pull(DOW)
slowest_day_count_ems <- min_dow_info_ems |> slice(1) |> pull(n)

# Create mapping from abbreviations to full day names
day_names <- c(
  "SUN" = "Sunday",
  "MON" = "Monday",
  "TUE" = "Tuesday",
  "WED" = "Wednesday",
  "THU" = "Thursday",
  "FRI" = "Friday",
  "SAT" = "Saturday"
)

# Convert abbreviations to full day names
busiest_day_ems <- day_names[busiest_day_abbr_ems]
slowest_day_ems <- day_names[slowest_day_abbr_ems]

barDOW_EMS <- df_ems |> ggplot(aes(x = DOW, fill = DOW)) +
    geom_bar() +
    paletteer::scale_fill_paletteer_d("ggsci::green_material") +
    labs(
        title = "Number of Calls for Service for EMS by Day of the Week",
        x = "Day of the Week",
        y = "Number of Calls"
    ) +
    geom_text(
        stat = "count",
        aes(label = after_stat(count)),
        vjust = -0.5,
        size = 6
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
    )

barDOW_EMS
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by hour of the day."
# ggplot2
hour_counts_ems <- df_ems |>
  count(Hour, sort = TRUE)

max_hour_info_ems <- hour_counts_ems |> filter(n == max(n))
busiest_hour_ems <- sprintf("%02d", max_hour_info_ems |> slice(1) |> pull(Hour))
busiest_hour_count_ems <- max_hour_info_ems |> slice(1) |> pull(n)

min_hour_info_ems <- hour_counts_ems |> filter(n == min(n))
slowest_hour_ems <- sprintf("%02d", min_hour_info_ems |> slice(1) |> pull(Hour))
slowest_hour_count_ems <- min_hour_info_ems |> slice(1) |> pull(n)

barHour_EMS <- df_ems |> ggplot(aes(x = Hour, fill = Hour)) +
    geom_bar() +
    {
        ems_base_cols <- tryCatch({
            as.character(paletteer::paletteer_d("cartography::green.pal"))
        }, error = function(e) {
            c("#00441B", "#006D2C", "#238B45", "#41AE76", "#66C2A4", "#99D8C9", "#C7E9C0", "#E5F5E0")
        })
        scale_fill_manual(values = grDevices::colorRampPalette(ems_base_cols)(nlevels(df_ems$Hour)))
    } +
    labs(
        title = "Number of Calls for Service for EMS by Hour of the Day",
        x = "Hour of the Day",
        y = "Number of Calls"
    ) +
    geom_text(
        stat = "count",
        aes(label = after_stat(count)),
        vjust = -0.5,
        size = 6
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
    )

barHour_EMS
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by call reception."
# ggplot2
cr_counts_ems <- df_ems |>
  count(Call_Reception, sort = TRUE)

max_cr_info_ems <- cr_counts_ems |> filter(n == max(n))
busiest_cr_ems <- max_cr_info_ems |> slice(1) |> pull(Call_Reception)
busiest_cr_count_ems <- max_cr_info_ems |> slice(1) |> pull(n)

cr_nr_pct_ems <- round((sum(df_ems$Call_Reception == "NOT CAPTURED") / nrow(df_ems)) * 100, 1)

barReception_EMS <- df_ems |> ggplot(aes(x = Call_Reception, fill = Call_Reception)) +
    geom_bar() +
    paletteer::scale_fill_paletteer_d("ggsci::green_material") +
    labs(
        title = "Number of Calls for Service for EMS by Call Reception",
        x = "Call Reception",
        y = "Number of Calls"
    ) +
    geom_text(
        stat = "count",
        aes(label = after_stat(count)),
        vjust = -0.5,
        size = 6
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
    )

barReception_EMS
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by call type."
# ggplot2
prob_counts_med <- df_ems |>
  count(Problem, sort = TRUE)

max_prob_info_med <- prob_counts_med |> filter(n == max(n))
busiest_prob_med <- max_prob_info_med |> slice(1) |> pull(Problem)
busiest_prob_count_med <- max_prob_info_med |> slice(1) |> pull(n)

ems_ma_call <- sum(startsWith(df_ems$Problem, "MUTUAL"), na.rm = TRUE)

problem_counts_EMS <- df_ems |>
  count(Problem, sort = TRUE) |>
  slice_head(n = 10)

barProblem_EMS <- problem_counts_EMS |>
    ggplot(aes(x = reorder(Problem, -n), y = n, fill = Problem)) +
    geom_bar(stat = "identity") +
    paletteer::scale_fill_paletteer_d("ggsci::green_material") +
    labs(
        title = "Number of Calls for Service for EMS by Call Type",
        x = "Call Type",
        y = "Number of Calls"
    ) +
    geom_text(
        aes(label = n),
        vjust = -0.5,
        size = 6
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
    )

barProblem_EMS
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by priority level."
# ggplot2
pri_counts_ems <- df_ems |>
  count(Priority_Number, sort = TRUE)

max_pri_info_ems <- pri_counts_ems |> filter(n == max(n))
busiest_pri_ems <- max_pri_info_ems |> slice(1) |> pull(Priority_Number)
busiest_pri_count_ems <- max_pri_info_ems |> slice(1) |> pull(n)

# Calculate percentage for APD priority calls
busiest_pri_ems_percentage <- round((sum(df_ems$Priority_Number == busiest_pri_ems, na.rm = TRUE) / nrow(df_ems)) * 100, 1)

# Calculate percentage for P1 calls
p1_ems_percentage <- round((sum(df_ems$Priority_Number == 1, na.rm = TRUE) / nrow(df_ems)) * 100, 1)

barPriority_EMS <- df_ems |> ggplot(aes(x = Priority_Number, fill = Priority_Number)) +
    geom_bar() +
    paletteer::scale_fill_paletteer_d("ggsci::green_material") +
    labs(
        title = "Number of Calls for Service by Priority Level",
        x = "Priority Level",
        y = "Number of Calls"
    ) +
    geom_text(
        stat = "count",
        aes(label = after_stat(count)),
        vjust = -0.5,
        size = 6
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
    )


barPriority_EMS
#
#
#
#
#
#| label: ems-custom-summary
#| echo: false
#| message: false
#| warning: false

# Initialize variables
summary_table <- NULL
median_processing_time <- NA_real_
median_phone_time <- NA_real_

# Create a summary table of elapsed time variables
# Create a summary table of elapsed time variables
tryCatch({
  # Define the columns we want to analyze
  time_columns <- c("Time_To_Queue", "Time_To_Dispatch", "Phone_Time",
                    "Processing_Time", "Rollout_Time", "Transit_Time", "Total_Call_Time")
  
  # Check which columns actually exist in the data
  existing_columns <- time_columns[time_columns %in% names(df_ems)]
  
  if (length(existing_columns) == 0) {
    cat("No time columns found in the data.\n")
  } else {
    
    summary_table <- df_ems %>%
      # 1. Select only the columns that exist
      dplyr::select(all_of(existing_columns)) %>%
      # 2. Summarize across all selected columns, converting difftime to numeric safely
      summarise(across(everything(),
        list(
          Minimum  = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(min(vals, na.rm = TRUE), 2)
          },
          Mean     = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(mean(vals, na.rm = TRUE), 2)
          },
          Median   = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(median(vals, na.rm = TRUE), 2)
          },
          Std_Dev  = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(sd(vals, na.rm = TRUE), 2)
          },
          Skewness = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else {
              tryCatch(round(psych::skew(vals, na.rm = TRUE), 2), error = function(e) NA_real_)
            }
          },
          Kurtosis = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals)) || length(vals[!is.na(vals)]) < 4) {
              NA_real_
            } else {
              tryCatch({
                # Remove NA values first
                clean_vals <- vals[!is.na(vals)]
                
                # Try multiple approaches for kurtosis calculation
                result <- tryCatch({
                  # Method 1: Use psych::kurtosis with explicit namespace
                  psych::kurtosis(clean_vals)
                }, error = function(e1) {
                  tryCatch({
                    # Method 2: Use moments package if available
                    if (requireNamespace("moments", quietly = TRUE)) {
                      moments::kurtosis(clean_vals) - 3  # Convert to excess kurtosis
                    } else {
                      stop("moments not available")
                    }
                  }, error = function(e2) {
                    # Method 3: Manual calculation
                    n <- length(clean_vals)
                    if (n < 4) return(NA_real_)
                    
                    mean_val <- mean(clean_vals)
                    var_val <- var(clean_vals)
                    
                    if (var_val == 0) return(NA_real_)
                    
                    # Calculate fourth moment
                    fourth_moment <- mean((clean_vals - mean_val)^4)
                    # Calculate kurtosis (excess kurtosis = kurtosis - 3)
                    kurt_val <- (fourth_moment / (var_val^2)) - 3
                    kurt_val
                  })
                })
                
                round(result, 2)
              }, error = function(e) {
                # Final fallback
                NA_real_
              })
            }
          }
        ),
        .names = "{.col}---{.fn}" # Use a unique separator
      )) %>%
      # 3. Reshape the data to a long format, then back to a clean wide format
      pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
      separate(Variable, into = c("Variable", "Statistic"), sep = "---") %>%
      pivot_wider(names_from = Statistic, values_from = Value) %>%
      mutate(Variable = str_replace_all(Variable, "_", " ")) # Clean up names for display
    
    # Extract key metrics for use in text
    median_time_to_queue <- summary_table |> 
      filter(Variable == "Time To Queue") |> 
      pull(Median)
    median_time_to_queue_ems <- median_time_to_queue
      
    median_time_to_dispatch <- summary_table |> 
      filter(Variable == "Time To Dispatch") |> 
      pull(Median)
    median_time_to_dispatch_ems <- median_time_to_dispatch

    median_processing_time <- summary_table |>
      filter(Variable == "Processing_Time") |>
      pull(Median)
    median_processing_time_ems <- median_processing_time

    median_phone_time <- summary_table |>
      filter(Variable == "Phone_Time") |>
      pull(Median)
    median_phone_time_ems <- median_phone_time

    ems_hp_calls <- df_ems |>
      dplyr::filter(Priority_Number <= 2)

    ems_64_calls <- df_ems |>
      dplyr::filter(Priority_Number <= 2 & Processing_Time <= 64)

    ems_106_calls <- df_ems |>
      dplyr::filter(Priority_Number <= 2 & Processing_Time <= 106)

    ems_64_compliance_pct <- round((nrow(ems_64_calls) / nrow(ems_hp_calls)) * 100, 1)
    ems_106_compliance_pct <- round((nrow(ems_106_calls) / nrow(ems_hp_calls)) * 100, 1)

    to_ft(
      summary_table,
      caption = "Weekly Elapsed Time Summary Table — Statistical summary of call processing times",
      header_map = list(
        Variable = "Time Metric",
        Minimum = "Min",
        Mean = "Mean",
        Median = "Median",
        Std_Dev = "Std Dev",
        Skewness = "Skew",
        Kurtosis = "Kurt"
      ),
      digits = 2
    )
  }
}, error = function(e) {
  cat("Error creating summary table:", e$message, "\n")
  cat("Available columns in df:", paste(names(df), collapse = ", "), "\n")
})
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: false
#| message: false
#| warning: false

# Table: Call_Taker frequency in df_ttq_delay (descending)
library(dplyr)
library(knitr)

call_taker_counts <- df_ttq_delay %>%
  count(Call_Taker, sort = TRUE)

to_ft(
  call_taker_counts,
  caption = "Frequency of Call Taker in Delayed TTQ Calls (Descending)",
  header_map = list(Call_Taker = "Call Taker", n = "Count"),
  digits = 0
)
#
#
#
#
#
#| echo: false
#| message: false
#| warning: false

# Table: Dispatcher frequency in df_ttd_delay (descending)
dispatcher_counts <- df_ttd_delay %>%
  count(Dispatcher, sort = TRUE)

to_ft(
  dispatcher_counts,
  caption = "Frequency of Dispatcher in Delayed TTD Calls (Descending)",
  header_map = list(Dispatcher = "Dispatcher", n = "Count"),
  digits = 0
)
#
#
#
#
#
#
#
#
#
#
#
#| echo: false
#| fig-cap: "Top High-Priority Call Types"
# ggplot2
hp_call_types <- df_hp |>
  count(Problem, sort = TRUE) |>
  slice_head(n = 10)

ggplot(hp_call_types, aes(x = reorder(Problem, n), y = n, fill = Problem)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  paletteer::scale_fill_paletteer_d("ggsci::deep_orange_material") +
  labs(title = "Top High-Priority Call Types",
       x = "Call Type",
       y = "Number of Calls") +
  geom_text(
    aes(label = n),
    hjust = -0.2,
    size = 5
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )
#
#
#
#
#
#
#
#| echo: false
#| fig-cap: "High-Priority Call Response Times"
#| warning: false
#| message: false
# ggplot2
hp_response_times <- df_hp |>
  transmute(
    Time_To_Queue = as.numeric(Time_To_Queue),
    Time_To_Dispatch = as.numeric(Time_To_Dispatch),
    Phone_Time = as.numeric(Phone_Time),
    Processing_Time = as.numeric(Processing_Time),
    Rollout_Time = as.numeric(Rollout_Time),
    Transit_Time = as.numeric(Transit_Time),
    Total_Call_Time = as.numeric(Total_Call_Time)
  ) |>
  pivot_longer(everything(), names_to = "Metric", values_to = "Time")
  
ggplot(hp_response_times |> filter(!is.na(Time) & Time >= 0), aes(x = Time)) +
  geom_histogram(binwidth = 5, fill = "#1c5789", color = "white", alpha = 0.7) +
  facet_wrap(~ Metric, scales = "free") +
  scale_x_continuous(labels = scales::comma) + # Improve readability of x-axis
  labs(title = "Distribution of Response Times for High-Priority Calls",
       x = "Time (seconds)",
       y = "Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )
#
#
#
#
#
#
#
#
#
#
#
#| label: e911-calls
#| echo: false
#| message: false
#| warning: false

# Initialize variables
summary_table <- NULL
median_processing_time <- NA_real_
median_phone_time <- NA_real_

# Create a summary table of elapsed time variables
# Create a summary table of elapsed time variables
tryCatch({
  # Define the columns we want to analyze
  time_columns <- c("Time_To_Queue", "Time_To_Dispatch", "Phone_Time",
                    "Processing_Time", "Rollout_Time", "Transit_Time", "Total_Call_Time")
  
  # Check which columns actually exist in the data
  existing_columns <- time_columns[time_columns %in% names(df_911)]
  
  if (length(existing_columns) == 0) {
    cat("No time columns found in the data.\n")
  } else {
    
    summary_table <- df_911 %>%
      # 1. Select only the columns that exist
      dplyr::select(all_of(existing_columns)) %>%
      # 2. Summarize across all selected columns, converting difftime to numeric safely
      summarise(across(everything(),
        list(
          Minimum  = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(min(vals, na.rm = TRUE), 2)
          },
          Mean     = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(mean(vals, na.rm = TRUE), 2)
          },
          Median   = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(median(vals, na.rm = TRUE), 2)
          },
          Std_Dev  = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(sd(vals, na.rm = TRUE), 2)
          },
          Skewness = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else {
              tryCatch(round(psych::skew(vals, na.rm = TRUE), 2), error = function(e) NA_real_)
            }
          },
          Kurtosis = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals)) || length(vals[!is.na(vals)]) < 4) {
              NA_real_
            } else {
              tryCatch({
                # Remove NA values first
                clean_vals <- vals[!is.na(vals)]
                
                # Try multiple approaches for kurtosis calculation
                result <- tryCatch({
                  # Method 1: Use psych::kurtosis with explicit namespace
                  psych::kurtosis(clean_vals)
                }, error = function(e1) {
                  tryCatch({
                    # Method 2: Use moments package if available
                    if (requireNamespace("moments", quietly = TRUE)) {
                      moments::kurtosis(clean_vals) - 3  # Convert to excess kurtosis
                    } else {
                      stop("moments not available")
                    }
                  }, error = function(e2) {
                    # Method 3: Manual calculation
                    n <- length(clean_vals)
                    if (n < 4) return(NA_real_)
                    
                    mean_val <- mean(clean_vals)
                    var_val <- var(clean_vals)
                    
                    if (var_val == 0) return(NA_real_)
                    
                    # Calculate fourth moment
                    fourth_moment <- mean((clean_vals - mean_val)^4)
                    # Calculate kurtosis (excess kurtosis = kurtosis - 3)
                    kurt_val <- (fourth_moment / (var_val^2)) - 3
                    kurt_val
                  })
                })
                
                round(result, 2)
              }, error = function(e) {
                # Final fallback
                NA_real_
              })
            }
          }
        ),
        .names = "{.col}---{.fn}" # Use a unique separator
      )) %>%
      # 3. Reshape the data to a long format, then back to a clean wide format
      pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
      separate(Variable, into = c("Variable", "Statistic"), sep = "---") %>%
      pivot_wider(names_from = Statistic, values_from = Value) %>%
      mutate(Variable = str_replace_all(Variable, "_", " ")) # Clean up names for display
    
    # Extract key metrics for use in text
  median_time_to_queue <- summary_table |>
  filter(Variable == "Time To Queue") |>
      pull(Median)
      
  median_time_to_dispatch <- summary_table |>
  filter(Variable == "Time To Dispatch") |>
      pull(Median)

    median_processing_time <- summary_table |>
      filter(Variable == "Processing_Time") |>
      pull(Median)

    median_phone_time <- summary_table |>
      filter(Variable == "Phone_Time") |>
      pull(Median)

    to_ft(
      summary_table,
      caption = "Weekly Elapsed Time Summary Table — Statistical summary of call processing times",
      header_map = list(
        Variable = "Time Metric",
        Minimum = "Min",
        Mean = "Mean",
        Median = "Median",
        Std_Dev = "Std Dev",
        Skewness = "Skew",
        Kurtosis = "Kurt"
      ),
      digits = 2
    )
  }
}, error = function(e) {
  cat("Error creating summary table:", e$message, "\n")
  cat("Available columns in df:", paste(names(df), collapse = ", "), "\n")
})

#
#
#
#
#
#| echo: false
# If overall aliases exist, restore the generic median variables to overall values
if (exists("median_time_to_queue_overall")) median_time_to_queue <- median_time_to_queue_overall
if (exists("median_time_to_dispatch_overall")) median_time_to_dispatch <- median_time_to_dispatch_overall
if (exists("median_processing_time_overall")) median_processing_time <- median_processing_time_overall
if (exists("median_phone_time_overall")) median_phone_time <- median_phone_time_overall
#
#
#
#
#
#| label: 911-call-day
#| echo: false
#| fig-cap: "E-911 Call Volume by Day of the Week"
# ggpl0t2

dow_counts_911 <- df_911 |>
  count(DOW, sort = TRUE)

max_dow_info_911 <- dow_counts_911 |> filter(n == max(n))
busiest_day_abbr_911 <- max_dow_info_911 |> slice(1) |> pull(DOW)
busiest_day_count_911 <- max_dow_info_911 |> slice(1) |> pull(n)

min_dow_info_911 <- dow_counts_911 |> filter(n == min(n))
slowest_day_abbr_911 <- min_dow_info_911 |> slice(1) |> pull(DOW)
slowest_day_count_911 <- min_dow_info_911 |> slice(1) |> pull(n)

# Create mapping from abbreviations to full day names
day_names <- c(
  "SUN" = "Sunday",
  "MON" = "Monday",
  "TUE" = "Tuesday",
  "WED" = "Wednesday",
  "THU" = "Thursday",
  "FRI" = "Friday",
  "SAT" = "Saturday"
)

# Convert abbreviations to full day names
busiest_day_911 <- day_names[busiest_day_abbr_911]
slowest_day_911 <- day_names[slowest_day_abbr_911]

barDOW_911 <- df_911 |> ggplot(aes(x = DOW, fill = DOW)) +
    geom_bar() +
    paletteer::scale_fill_paletteer_d("ggsci::indigo_material") +
    labs(
        title = "Number of Calls for Service for 911 cals by Day of the Week",
        x = "Day of the Week",
        y = "Number of Calls"
    ) +
    geom_text(
        stat = "count",
        aes(label = after_stat(count)),
        vjust = -0.5,
        size = 6
    ) +
    theme_minimal() +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
    )

barDOW_911
#
#
#
#
#
#| label: 911-call-hour
#| echo: false
#| fig-cap: "E-911 Call Volume by Hour of the Day"
#| warning: false
# ggplot2

hour_counts_911 <- df_911 |>
  count(Hour, sort = TRUE)

max_hour_info_911 <- hour_counts_911 |> filter(n == max(n))
busiest_hour_911 <- sprintf("%02d", max_hour_info_911 |> slice(1) |> pull(Hour))
busiest_hour_count_911 <- max_hour_info_911 |> slice(1) |> pull(n)

min_hour_info_911 <- hour_counts_911 |> filter(n == min(n))
slowest_hour_911 <- sprintf("%02d", min_hour_info_911 |> slice(1) |> pull(Hour))
slowest_hour_count_911 <- min_hour_info_911 |> slice(1) |> pull(n)

barHour <- df_911 |> ggplot(aes(x=Hour, fill=Hour)) +
  geom_bar() +
  # Use ggsci::indigo_material palette with robust fallback
  {
    indigo_material_cols <- tryCatch(
      {
        as.character(paletteer::paletteer_d("ggsci::indigo_material"))
      },
      error = function(e) {
        # Fallback indigo colors if palette not available
        c("#1A237E", "#283593", "#3949AB", "#3F51B5", "#5C6BC0", "#7986CB", "#9FA8DA", "#C5CAE9")
      }
    )
    scale_fill_manual(values = grDevices::colorRampPalette(indigo_material_cols)(nlevels(df_911$Hour)))
  } +
  labs(title="Number of Calls for Service by Hour of the Day",
       x="Hour of the Day",
       y="Number of Calls") +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size=6
  ) +
  theme_minimal() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size=18),
        axis.text.x = element_text(angle=45, hjust=1, size=12),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

barHour
#
#
#
#
#
#
#
#
#
#| echo: false
#| fig-cap: "Cardiac Arrest Call Volume by Day and Hour"
# ggplot2
barDOW_CA <- df_ca |> ggplot(aes(x=DOW, fill=DOW)) +
  geom_bar() +
  paletteer::scale_fill_paletteer_d("ggsci::light_green_material") +
  labs(title="Number of Calls for Service for Cardiac Arrest by Day of the Week",
       x="Day of the Week",
       y="Number of Calls") +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size=6
  ) +
  theme_minimal() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size=18),
        axis.text.x = element_text(angle=45, hjust=1, size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

barDOW_CA
#
#
#
#
#
#| echo: false
#| fig-cap: "Cardiac Arrest Call Response Times"
#| warning: false
#| message: false
tryCatch({
  # Define the columns we want to analyze
  time_columns <- c("Time_To_Queue", "Time_To_Dispatch", "Phone_Time",
                    "Processing_Time", "Rollout_Time", "Transit_Time", "Total_Call_Time")

  # Check which columns actually exist in the data
  existing_columns <- time_columns[time_columns %in% names(df_ca)]

  if (length(existing_columns) == 0) {
    cat("No time columns found in the data.\n")
  } else {

    summary_table <- df_ca %>%
      # 1. Select only the columns that exist
      dplyr::select(all_of(existing_columns)) %>%
      # 2. Summarize across all selected columns, converting difftime to numeric safely
      summarise(across(everything(),
        list(
          Minimum  = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(min(vals, na.rm = TRUE), 2)
          },
          Mean     = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(mean(vals, na.rm = TRUE), 2)
          },
          Median   = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(median(vals, na.rm = TRUE), 2)
          },
          Std_Dev  = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(sd(vals, na.rm = TRUE), 2)
          },
          Skewness = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else {
              tryCatch(round(psych::skew(vals, na.rm = TRUE), 2), error = function(e) NA_real_)
            }
          },
          Kurtosis = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals)) || length(vals[!is.na(vals)]) < 4) {
              NA_real_
            } else {
              tryCatch({
                # Remove NA values first
                clean_vals <- vals[!is.na(vals)]

                # Try multiple approaches for kurtosis calculation
                result <- tryCatch({
                  # Method 1: Use psych::kurtosis with explicit namespace
                  psych::kurtosis(clean_vals)
                }, error = function(e1) {
                  tryCatch({
                    # Method 2: Use moments package if available
                    if (requireNamespace("moments", quietly = TRUE)) {
                      moments::kurtosis(clean_vals) - 3  # Convert to excess kurtosis
                    } else {
                      stop("moments not available")
                    }
                  }, error = function(e2) {
                    # Method 3: Manual calculation
                    n <- length(clean_vals)
                    if (n < 4) return(NA_real_)

                    mean_val <- mean(clean_vals)
                    var_val <- var(clean_vals)

                    if (var_val == 0) return(NA_real_)

                    # Calculate fourth moment
                    fourth_moment <- mean((clean_vals - mean_val)^4)
                    # Calculate kurtosis (excess kurtosis = kurtosis - 3)
                    kurt_val <- (fourth_moment / (var_val^2)) - 3
                    kurt_val
                  })
                })

                round(result, 2)
              }, error = function(e) {
                # Final fallback
                NA_real_
              })
            }
          }
        ),
        .names = "{.col}---{.fn}" # Use a unique separator
      )) %>%
      # 3. Reshape the data to a long format, then back to a clean wide format
      pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
      separate(Variable, into = c("Variable", "Statistic"), sep = "---") %>%
      pivot_wider(names_from = Statistic, values_from = Value) %>%
      mutate(Variable = str_replace_all(Variable, "_", " ")) # Clean up names for display

     #Extract key metrics for use in text
  median_time_to_queue <- summary_table |>
  filter(Variable == "Time To Queue") |>
      pull(Median)
      
  median_time_to_dispatch <- summary_table |>
  filter(Variable == "Time To Dispatch") |>
      pull(Median)

    median_processing_time <- summary_table |>
      filter(Variable == "Processing_Time") |>
      pull(Median)

    median_phone_time <- summary_table |>
      filter(Variable == "Phone_Time") |>
      pull(Median)

    to_ft(
      summary_table,
      caption = "Weekly Elapsed Time Summary Table — Statistical summary of call processing times",
      header_map = list(
        Variable = "Time Metric",
        Minimum = "Min",
        Mean = "Mean",
        Median = "Median",
        Std_Dev = "Std Dev",
        Skewness = "Skew",
        Kurtosis = "Kurtosis"
      ),
      digits = 2
    )
  }
}, error = function(e) {
  cat("Error creating summary table:", e$message, "\n")
  cat("Available columns in df:", paste(names(df), collapse = ", "), "\n")
})
#
#
#
#
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by day of the week."
# ggplot2
dow_counts_mh <- df_mh |>
  count(DOW, sort = TRUE)

max_dow_info_mh <- dow_counts_mh |> filter(n == max(n))
busiest_day_abbr_mh <- max_dow_info_mh |> slice(1) |> pull(DOW)
busiest_day_count_mh <- max_dow_info_mh |> slice(1) |> pull(n)

min_dow_info_mh <- dow_counts_mh |> filter(n == min(n))
slowest_day_abbr_mh <- min_dow_info_mh |> slice(1) |> pull(DOW)
slowest_day_count_mh <- min_dow_info_mh |> slice(1) |> pull(n)

# Create mapping from abbreviations to full day names
day_names <- c(
  "SUN" = "Sunday",
  "MON" = "Monday",
  "TUE" = "Tuesday",
  "WED" = "Wednesday",
  "THU" = "Thursday",
  "FRI" = "Friday",
  "SAT" = "Saturday"
)

# Convert abbreviations to full day names
busiest_day_mh <- day_names[busiest_day_abbr_mh]
slowest_day_mh <- day_names[slowest_day_abbr_mh]

barDOW_MH <- df_mh |> ggplot(aes(x=DOW, fill=DOW)) +
  geom_bar() +
  paletteer::scale_fill_paletteer_d("ggsci::purple_material") +
  labs(title="Number of Mental Health related calls by Day of the Week",
       x="Day of the Week",
       y="Number of Calls") +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size=6
  ) +
  theme_minimal() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size=18),
        axis.text.x = element_text(angle=45, hjust=1, size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

barDOW_MH
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by hour of the day."
# ggplot2
hour_counts_mh <- df_mh |>
  count(Hour, sort = TRUE)

max_hour_info_mh <- hour_counts_mh |> filter(n == max(n))
busiest_day_mh <- max_hour_info_mh |> slice(1) |> pull(Hour)
busiest_day_count_mh <- max_hour_info_mh |> slice(1) |> pull(n)

min_hour_info_mh <- hour_counts_mh |> filter(n == min(n))
slowest_day_mh <- min_hour_info_mh |> slice(1) |> pull(Hour)
slowest_day_count_mh <- min_hour_info_mh |> slice(1) |> pull(n)

barHour_MH <- df_mh |> ggplot(aes(x=Hour, fill=Hour)) +
  geom_bar() +
  # Use ggsci::purple_material palette scaled to 24 levels for hours
  {
    purple_material_cols <- tryCatch(
      {
        as.character(paletteer::paletteer_d("ggsci::purple_material"))
      },
      error = function(e) {
        # Fallback purple colors if palette not available
        c("#F3E5F5", "#E1BEE7", "#CE93D8", "#BA68C8", "#AB47BC", "#9C27B0", "#8E24AA", "#7B1FA2", "#6A1B9A", "#4A148C")
      }
    )
    scale_fill_manual(values = grDevices::colorRampPalette(purple_material_cols)(24))
  } +
  labs(title="Number of Calls for Service for APD by Hour of the Day",
       x="Hour of the Day",
       y="Number of Calls") +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size=6
  ) +
  theme_minimal() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size=18),
        axis.text.x = element_text(angle=45, hjust=1, size=12),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

barHour_MH
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by call reception."
# ggplot2
cr_counts_mh <- df_mh |>
  count(Call_Reception, sort = TRUE)

max_cr_info_mh <- cr_counts_mh |> filter(n == max(n))
busiest_cr_mh <- max_cr_info_mh |> slice(1) |> pull(Call_Reception)
busiest_cr_count_mh <- max_cr_info_mh |> slice(1) |> pull(n)

min_cr_info_mh <- cr_counts_mh |> filter(n == min(n))
slowest_cr_mh <- min_cr_info_mh |> slice(1) |> pull(Call_Reception)
slowest_cr_count_mh <- min_cr_info_mh |> slice(1) |> pull(n)

barReception_MH <- df_mh |> ggplot(aes(x=Call_Reception, fill=Call_Reception)) +
  geom_bar() +
  scale_fill_viridis(discrete=TRUE, option="G") +
  labs(title="Number of Calls for Service for by Call Reception",
       x="Call Reception",
       y="Number of Calls") +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size=6
  ) +
  theme_minimal() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size=18),
        axis.text.x = element_text(angle=45, hjust=1, size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

barReception_MH
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by call type."
# ggplot2
ct_counts_mh <- df_mh |>
  count(Problem, sort = TRUE)

max_ct_info_mh <- ct_counts_mh |> filter(n == max(n))
busiest_ct_mh <- max_ct_info_mh |> slice(1) |> pull(Problem)
busiest_ct_count_mh <- max_ct_info_mh |> slice(1) |> pull(n)

min_ct_info_mh <- ct_counts_mh |> filter(n == min(n))
slowest_ct_mh <- min_ct_info_mh |> slice(1) |> pull(Problem)
slowest_ct_count_mh <- min_ct_info_mh |> slice(1) |> pull(n)

problem_counts_MH <- df_mh |>
  count(Problem, sort = TRUE) |>
  slice_head(n = 10)

barProblem_MH <- problem_counts_MH |>
  ggplot(aes(x=reorder(Problem, -n), y=n, fill=Problem)) +
  geom_bar(stat="identity") +
  scale_fill_viridis(discrete=TRUE, option="G") +
  labs(title="Number of Calls for Service by Call Type",
       x="Call Type",
       y="Number of Calls") +
    geom_text(
        aes(label = n),
        vjust = -0.7,
        size = 5) +
    theme_minimal(base_size = 14) +
    theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12)
    )

barProblem_MH
#
#
#
#
#
#| echo: false
#| fig-cap: "Number of calls for service by priority level."
# ggplot2
pri_counts_mh <- df_mh |>
  count(Priority_Number, sort = TRUE)

max_pri_info_mh <- pri_counts_mh |> filter(n == max(n))
busiest_pri_mh <- max_pri_info_mh |> slice(1) |> pull(Priority_Number)
busiest_pri_count_mh <- max_pri_info_mh |> slice(1) |> pull(n)

min_pri_info_mh <- pri_counts_mh |> filter(n == min(n))
slowest_pri_mh <- min_pri_info_mh |> slice(1) |> pull(Priority_Number)
slowest_pri_count_mh <- min_pri_info_mh |> slice(1) |> pull(n)

barPriority_MH <- df_mh |> ggplot(aes(x=Priority_Number, fill=Priority_Number)) +
  geom_bar() +
  scale_fill_viridis(discrete=TRUE, option="G") +
  labs(title="Number of Calls for Service by Priority Level",
       x="Priority Level",
       y="Number of Calls") +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.5,
    size=6
  ) +
  theme_minimal() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size=18),
        axis.text.x = element_text(angle=45, hjust=1, size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))

barPriority_MH
#
#
#
#
#
#| label: mh-custom-summary
#| echo: false
#| message: false
#| warning: false

# Initialize variables
summary_table <- NULL
median_processing_time <- NA_real_

# Create a summary table of elapsed time variables
tryCatch({
  # Define the columns we want to analyze
  time_columns <- c("Time_To_Queue", "Time_To_Dispatch", "Phone_Time",
                    "Processing_Time", "Rollout_Time", "Transit_Time", "Total_Call_Time")

  # Check which columns actually exist in the data
  existing_columns <- time_columns[time_columns %in% names(df_mh)]

  if (length(existing_columns) == 0) {
    cat("No time columns found in the data.\n")
  } else {

    summary_table <- df_mh %>%
      # 1. Select only the columns that exist
      dplyr::select(all_of(existing_columns)) %>%
      # 2. Summarize across all selected columns, converting difftime to numeric safely
      summarise(across(everything(),
        list(
          Minimum  = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(min(vals, na.rm = TRUE), 2)
          },
          Mean     = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(mean(vals, na.rm = TRUE), 2)
          },
          Median   = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(median(vals, na.rm = TRUE), 2)
          },
          Std_Dev  = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else round(sd(vals, na.rm = TRUE), 2)
          },
          Skewness = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals))) NA_real_ else {
              tryCatch(round(psych::skew(vals, na.rm = TRUE), 2), error = function(e) NA_real_)
            }
          },
          Kurtosis = ~ {
            vals <- as.numeric(.)
            if (all(is.na(vals)) || length(vals[!is.na(vals)]) < 4) {
              NA_real_
            } else {
              tryCatch({
                # Remove NA values first
                clean_vals <- vals[!is.na(vals)]

                # Try multiple approaches for kurtosis calculation
                result <- tryCatch({
                  # Method 1: Use psych::kurtosis with explicit namespace
                  psych::kurtosis(clean_vals)
                }, error = function(e1) {
                  tryCatch({
                    # Method 2: Use moments package if available
                    if (requireNamespace("moments", quietly = TRUE)) {
                      moments::kurtosis(clean_vals) - 3  # Convert to excess kurtosis
                    } else {
                      stop("moments not available")
                    }
                  }, error = function(e2) {
                    # Method 3: Manual calculation
                    n <- length(clean_vals)
                    if (n < 4) return(NA_real_)

                    mean_val <- mean(clean_vals)
                    var_val <- var(clean_vals)

                    if (var_val == 0) return(NA_real_)

                    # Calculate fourth moment
                    fourth_moment <- mean((clean_vals - mean_val)^4)
                    # Calculate kurtosis (excess kurtosis = kurtosis - 3)
                    kurt_val <- (fourth_moment / (var_val^2)) - 3
                    kurt_val
                  })
                })

                round(result, 2)
              }, error = function(e) {
                # Final fallback
                NA_real_
              })
            }
          }
        ),
        .names = "{.col}---{.fn}" # Use a unique separator
      )) %>%
      # 3. Reshape the data to a long format, then back to a clean wide format
      pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
      separate(Variable, into = c("Variable", "Statistic"), sep = "---") %>%
      pivot_wider(names_from = Statistic, values_from = Value) %>%
      mutate(Variable = str_replace_all(Variable, "_", " ")) # Clean up names for display

    #Extract key metrics for use in text
  median_time_to_queue <- summary_table |>
  filter(Variable == "Time To Queue") |>
      pull(Median)
      
  median_time_to_dispatch <- summary_table |>
  filter(Variable == "Time To Dispatch") |>
      pull(Median)

    median_processing_time <- summary_table |>
      filter(Variable == "Processing_Time") |>
      pull(Median)

    median_phone_time <- summary_table |>
      filter(Variable == "Phone_Time") |>
      pull(Median)

    to_ft(
      summary_table,
      caption = "Weekly Elapsed Time Summary Table — Statistical summary of call processing times",
      header_map = list(
        Variable = "Time Metric",
        Minimum = "Min",
        Mean = "Mean",
        Median = "Median",
        Std_Dev = "Std Dev",
        Skewness = "Skew",
        Kurtosis = "Kurt"
      ),
      digits = 2
    )
  }
}, error = function(e) {
  cat("Error creating summary table:", e$message, "\n")
  cat("Available columns in df:", paste(names(df), collapse = ", "), "\n")
})
#
#
#
#
#
#
#
#
#
#
