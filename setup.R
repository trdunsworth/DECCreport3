# setup.R
# Robust package setup for Windows + clear logging

required_packages <- c(
  "tidyverse", "tidymodels", "devtools", "remotes", "ggpubr",
  "ggrepel", "ggraph", "gt", "gtExtras", "GGally", "rstatix", "car", "survival", "GPfit",
  "janitor", "Hmisc", "psych", "corrr", "ggcorrplot", "ggthemes", "lubridate",
  "ggridges", "multcomp", "emmeans", "RVAideMemoire", "FactoMineR", "DescTools", "nlme",
  "funModeling", "inspectdf", "dlookr", "viridis", "merTools", "factoextra", "nortest", "MASS",
  "randtests", "summarytools", "report", "knitr", "kableExtra", "dbus",
  "modelbased", "parameters", "performance", "insight", "paletteer", "flextable", "officer", "outliers"
)

is_windows <- identical(tolower(Sys.info()[["sysname"]]), "windows")
if (is_windows) {
  required_packages <- setdiff(required_packages, "dbus")
  # Prefer precompiled binaries on Windows
  options(pkgType = "binary")
  # More forgiving install behavior
  Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")
  # Use libcurl (wininet deprecated as of R 4.5)
  options(download.file.method = "libcurl")
}

# Use Posit Package Manager for stable binaries
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))

installed <- rownames(installed.packages())
to_install <- setdiff(required_packages, installed)

install_fail <- character(0)

if (length(to_install)) {
  message("Installing: ", paste(to_install, collapse = ", "))
  for (pkg in to_install) {
    message(sprintf("-> Installing %s ...", pkg))
    ok <- tryCatch({
      install.packages(pkg, dependencies = TRUE, Ncpus = max(1L, getOption("Ncpus", 2L)))
      TRUE
    }, error = function(e) {
      message(sprintf("!! Failed to install %s: %s", pkg, conditionMessage(e)))
      install_fail <<- c(install_fail, pkg)
      FALSE
    }, warning = function(w) {
      # Treat warnings as non-fatal; log and continue (avoid muffleWarning restart)
      message(sprintf("!! Warning while installing %s: %s", pkg, conditionMessage(w)))
      NULL
    })
  }
} else {
  message("All required packages already installed.")
}

# Verify loadability and report any failures
load_fail <- character(0)
for (pkg in required_packages) {
  ok <- suppressPackageStartupMessages(suppressWarnings(require(pkg, character.only = TRUE, quietly = TRUE)))
  if (!ok) load_fail <- c(load_fail, pkg)
}

message("----- Summary -----")
if (length(install_fail)) {
  message("Install failures: ", paste(unique(install_fail), collapse = ", "))
} else {
  message("No install failures.")
}
if (length(load_fail)) {
  message("Load failures: ", paste(unique(load_fail), collapse = ", "))
} else {
  message("All required packages loaded successfully.")
}

# Helpful hints for common Windows issues
if (is_windows) {
  # Build tools needed if any compiled package still fails
  if (length(load_fail)) {
    has_build <- tryCatch({
      suppressPackageStartupMessages(suppressWarnings(require(pkgbuild, quietly = TRUE)))
      pkgbuild::has_build_tools(debug = FALSE)
    }, error = function(e) NA)
    if (isFALSE(has_build)) {
      message("Note: Some packages may require Rtools (Windows build tools).")
      message("Download: https://cran.r-project.org/bin/windows/Rtools/")
    }
  }
}

invisible(NULL)
