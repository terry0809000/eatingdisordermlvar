required_pkgs <- c(
  "dplyr",
  "tidyr",
  "readr",
  "tibble",
  "lubridate",
  "lme4",
  "qgraph",
  "corpcor",
  "purrr"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs)
} else {
  message("All required packages are already installed.")
}
