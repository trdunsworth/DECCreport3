# Plot helpers shared across reports
# Use in .qmd by sourcing near the top: source("R/plot_helpers.R")

# DOCX-aware size helper
# html: size to use for HTML
# docx: optional size override for DOCX; if NULL, uses html
# Detects output format via knitr option set by Quarto/Pandoc
sz <- function(html, docx = NULL) {
  if (is.null(docx)) return(html)
  fmt <- tryCatch(knitr::opts_knit$get("rmarkdown.pandoc.to"), error = function(e) NULL)
  if (!is.null(fmt) && grepl("docx", fmt, ignore.case = TRUE)) return(docx)
  html
}
