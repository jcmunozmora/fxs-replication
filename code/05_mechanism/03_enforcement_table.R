## FxS 2026 — Mechanism DiD Table (Enforcement)
## Original: 05_mechanism/tables/03_tbl_dif-dif-mechanism.R (enforcement section)

rm(list = ls())
source(here::here("code/00_packages.R"))

df <- import(here("data/panel_grids_regressions.rds")) |>
  mutate(
    treat       = treat_staggered,
    log_mapmuse = log(mapmuse + 1)
  )

samp <- df |> subset(sample_control_1 == 1 & year %in% 2011:2021)

n_control <- df |> subset(sample_control_1 == 1 & d_control_1 == 1) |>
               distinct(id_grid) |> nrow()
n_treated <- df |> subset(sample_control_1 == 1 & d_treated_1 == 1) |>
               distinct(id_grid) |> nrow()

mean_erradi  <- samp |> subset(d_control_1 == 1 & treat_staggered == 0) |>
                  pull(erradi_h) |> mean(na.rm = TRUE) |> round(3)
mean_mapmuse <- samp |> subset(d_control_1 == 1 & treat_staggered == 0) |>
                  pull(log_mapmuse) |> mean(na.rm = TRUE) |> round(3)

m1 <- feols(erradi_h   ~ treat | id_grid + year, data = samp, cluster = ~id_grid)
m2 <- feols(erradi_h   ~ treat + as.factor(provincia):as.factor(year) | id_grid + year,
            data = samp, cluster = ~id_grid)
m3 <- feols(log_mapmuse ~ treat | id_grid + year, data = samp, cluster = ~id_grid)
m4 <- feols(log_mapmuse ~ treat + as.factor(provincia):as.factor(year) | id_grid + year,
            data = samp, cluster = ~id_grid)

etable(
  m1, m2, m3, m4,
  keep_raw = "^treat$",
  dict     = c("treat" = "\\text{FxS}"),
  tex      = TRUE,
  file     = here("output/tables/mechanisms/enforcement_2011_2021.tex"),
  digits   = 3,
  se.below = TRUE,
  fitstat  = ~ n,
  extralines = list(
    "Mean Dep. Var."   = c(rep(mean_erradi, 2), rep(mean_mapmuse, 2)),
    "N. Treated"       = rep(n_treated, 4),
    "N. Control"       = rep(n_control, 4),
    "T.E. (Prov-Year)" = c("No","Yes","No","Yes"),
    "Period"           = rep("2011-2021", 4)
  ),
  headers = list("Eradication (ha)" = 2, "Drug Seizures (log)" = 2)
)

cat("Enforcement coefficients:\n")
etable(m1, m2, m3, m4, keep_raw = "^treat$")
cat("✓ Enforcement table saved\n")
