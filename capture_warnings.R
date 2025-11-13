options(warn = 0)

suppressPackageStartupMessages(library(knitr))

status <- try(knit('test_base_comparison.qmd', 'knit_for_warnings.md'))

w <- warnings()

if (!is.null(w)) {
  sink('warnings.txt')
  print(w)
  sink()
} else {
  write('No warnings captured', file = 'warnings.txt')
}
