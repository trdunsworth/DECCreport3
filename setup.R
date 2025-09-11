# setup.R
# This script checks for and installs all necessary packages for the report.
# Run this script once, or whenever you need to update/add packages.

required_packages <- c(
  "tidyverse", "tidymodels", "devtools", "remotes", "ggpubr",
  "ggrepel", "ggraph", "gt", "gtExtras", "GGally", "rstatix", "car", "survival",
  "janitor", "Hmisc", "psych", "corrr", "ggcorrplot", "ggthemes", "lubridate",
  "ggridges", "multcomp", "emmeans", "RVAideMemoire", "FactoMineR", "DescTools", "nlme", "funModeling", "inspectdf", "dlookr", "viridis", "merTools", "factoextra", "nortest", "MASS",
  "randtests", "summarytools", "report", "knitr", "kableExtra",
  "modelbased", "parameters", "performance", "insight", "ggridges"
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if(length(new_packages)) {
  install.packages(new_packages, repos = "https://packagemanager.posit.co/cran/latest")
}

message("All required packages are installed.")
