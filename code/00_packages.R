## FxS 2026 Replication Package — Package Setup
## R version 4.3.3
## Migration: lfe::felm() → fixest::feols() (lfe does not compile on Apple Silicon)

if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  tidyverse,      # data wrangling
  rio,            # data I/O
  here,           # portable paths
  fixest,         # TWFE (replaces lfe)
  fastDummies,    # dummy_cols()
  bacondecomp,    # Goodman-Bacon decomposition
  broom,          # tidy model output
  lubridate,
  janitor,
  skimr,
  ggplot2,
  sf,
  modelsummary    # table output supporting fixest
)

## resolve conflicts
filter  <- dplyr::filter
select  <- dplyr::select
rename  <- dplyr::rename
arrange <- dplyr::arrange

sf_use_s2(FALSE)

## ── felm compatibility wrapper ──────────────────────────────────────────────
## Translates lfe::felm(y ~ x | FE | 0 | cluster) → fixest::feols()
## Usage is identical to the original scripts.
felm <- function(formula, data = NULL, ...) {

  fstr  <- deparse(formula, width.cutoff = 500)
  parts <- strsplit(fstr, " \\| ")[[1]]

  main     <- trimws(parts[1])
  fe_part  <- if (length(parts) >= 2) trimws(parts[2]) else "0"
  clus_var <- if (length(parts) >= 4) trimws(parts[4]) else NULL

  # build fixest formula
  if (fe_part != "0" && nchar(fe_part) > 0) {
    new_fml <- as.formula(paste(main, "|", fe_part))
  } else {
    new_fml <- as.formula(main)
  }

  # cluster argument
  clus <- if (!is.null(clus_var) && clus_var != "0") {
    as.formula(paste("~", clus_var))
  } else {
    "iid"
  }

  if (is.null(data)) {
    feols(new_fml, cluster = clus, ...)
  } else {
    feols(new_fml, data = data, cluster = clus, ...)
  }
}

## ── etable helper ───────────────────────────────────────────────────────────
## Thin wrapper around fixest::etable() with project defaults
fxs_table <- function(models, file, coef_rename = NULL, extra_rows = NULL,
                      col_labels = NULL, digits = 3) {
  etable(
    .list     = models,
    file      = file,
    tex       = TRUE,
    digits    = digits,
    dict      = coef_rename,
    extralines = extra_rows,
    se.below  = TRUE,
    fitstat   = ~ n + r2,
    style.tex = style.tex("base", depvar.title = "", yesNo = c("Yes", "No"))
  )
  invisible(NULL)
}
