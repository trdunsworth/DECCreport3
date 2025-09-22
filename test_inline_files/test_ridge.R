# Test script to check ridge plot functionality
library(tidyverse)
library(ggridges)

# Load data
df <- read_csv("data/week35.csv")

# Process data
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

df$Hour <- as.factor(df$Hour)
df$DOW <- factor(
    df$DOW,
    levels = c("SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT"),
    ordered = TRUE
)

# Create summary data for ridge plot
hourly_dow_data <- df |>
  group_by(DOW, Hour) |>
  summarise(call_count = n(), .groups = 'drop') |>
  mutate(Hour_numeric = as.numeric(as.character(Hour)))

print("Data summary:")
print(head(hourly_dow_data))
print(paste("Data dimensions:", nrow(hourly_dow_data), "x", ncol(hourly_dow_data)))

# Test basic plot
cat("Testing basic ridge plot...\n")
