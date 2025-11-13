# setup.R
# Robust package setup for Windows + clear logging

required_packages <- c(
  # Core data wrangling & modeling
  "tidyverse", "tidymodels", "lubridate", "stringr", "readr", "vroom",
  # Visualization & reporting
  "ggpubr", "ggrepel", "ggraph", "GGally", "ggthemes", "ggridges", "viridis", "paletteer", "patchwork", "plotly",
  # Tables & formatting
  "gt", "gtExtras", "flextable", "officer", "kableExtra", "rmarkdown", "knitr",
  # Stats & inference
  "rstatix", "car", "survival", "GPfit", "multcomp", "emmeans", "RVAideMemoire", "FactoMineR", "DescTools", "nlme", "nortest", "randtests", "outliers",
  # Modeling helpers & diagnostics
  "funModeling", "inspectdf", "dlookr", "merTools", "factoextra", "modelbased", "parameters", "performance", "insight", "report", "summarytools",
  # Data hygiene & misc
  "janitor", "Hmisc", "psych", "corrr", "ggcorrplot", "reticulate", "MASS",
  # Shiny dashboard
  "shiny", "bslib"
)

# Packages that need to be installed from GitHub
github_packages <- list(
  ggradar = "ricardo-bion/ggradar"
)

is_windows <- identical(tolower(Sys.info()[["sysname"]]), "windows")
if (is_windows) {
  required_packages <- setdiff(required_packages, "dbus")  # Remove if accidentally included
  options(pkgType = "binary")
  Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")
  options(download.file.method = "libcurl")
  # Ensure versioned user library path exists for new R version (e.g., 4.5)
  ver_main <- paste0(R.version$major, ".", strsplit(R.version$minor, "\\.")[[1]][1])
  user_lib <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", ver_main)
  if (!dir.exists(user_lib)) {
    dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
    message("Created user library directory: ", user_lib)
  }
  .libPaths(c(user_lib, .libPaths()))
  message("Using .libPaths(): ", paste(.libPaths(), collapse = " | "))
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

# Install GitHub packages
for (pkg_name in names(github_packages)) {
  if (!pkg_name %in% installed) {
    message(sprintf("-> Installing %s from GitHub (%s) ...", pkg_name, github_packages[[pkg_name]]))
    ok <- tryCatch({
      remotes::install_github(github_packages[[pkg_name]], dependencies = TRUE, upgrade = "never")
      TRUE
    }, error = function(e) {
      message(sprintf("!! Failed to install %s from GitHub: %s", pkg_name, conditionMessage(e)))
      install_fail <<- c(install_fail, pkg_name)
      FALSE
    }, warning = function(w) {
      message(sprintf("!! Warning while installing %s from GitHub: %s", pkg_name, conditionMessage(w)))
      NULL
    })
  } else {
    message(sprintf("%s already installed (GitHub package)", pkg_name))
  }
}

# Verify loadability and report any failures
load_fail <- character(0)
all_packages <- c(required_packages, names(github_packages))
for (pkg in all_packages) {
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
if (is_windows && length(load_fail)) {
  has_build <- tryCatch({
    suppressPackageStartupMessages(suppressWarnings(require(pkgbuild, quietly = TRUE)))
    pkgbuild::has_build_tools(debug = FALSE)
  }, error = function(e) NA)
  if (isFALSE(has_build)) {
    message("Note: Some packages may require Rtools (Windows build tools).")
    message("Download: https://cran.r-project.org/bin/windows/Rtools/")
  }
}

invisible(NULL)
