## FxS 2026 — Main Regression Table (TWFE)
## Original: 04_main_regressions/tables/01_main-reg_crops_2011_2021.R

rm(list = ls())
source(here::here("code/00_packages.R"))

df <- import(here("data/panel_grids_regressions.rds"))

## ── sample stats ─────────────────────────────────────────────────────────────
n_control    <- df |> subset(sample_control_1 == 1 & d_control_1 == 1) |>
                  distinct(id_grid) |> nrow()
n_treated    <- df |> subset(sample_control_1 == 1 & d_treated_1 == 1) |>
                  distinct(id_grid) |> nrow()
mean_dep_var <- df |> subset(year %in% 2011:2021 & sample_control_1 == 1 &
                               d_control_1 == 1 & treat_staggered == 0) |>
                  pull(crops) |> mean(na.rm = TRUE) |> round(4)

## ── extensive margin ─────────────────────────────────────────────────────────
samp1 <- df |> subset(sample_control_1 == 1 & year %in% 2011:2021) |>
           rename(treat = treat_staggered)

m1 <- feols(crops ~ treat | id_grid + year, data = samp1, cluster = ~id_grid)
m2 <- feols(crops ~ treat + as.factor(provincia):as.factor(year) | id_grid + year,
            data = samp1, cluster = ~id_grid)

## ── intensive margin (treat = share of titled ha) ────────────────────────────
samp1_int <- samp1 |>
  mutate(treat = ifelse(treat == 1, n_ha_in_grid, 0))

m3 <- feols(crops ~ treat | id_grid + year, data = samp1_int, cluster = ~id_grid)
m4 <- feols(crops ~ treat + as.factor(provincia):as.factor(year) | id_grid + year,
            data = samp1_int, cluster = ~id_grid)

## ── table ─────────────────────────────────────────────────────────────────────
etable(
  m1, m2, m3, m4,
  keep_raw = "^treat$",
  dict     = c("treat" = "\\text{FxS}"),
  tex      = TRUE,
  file     = here("output/tables/main-regressions/main_reg_crops_2011_2021.tex"),
  digits   = 3,
  se.below = TRUE,
  fitstat  = ~ n,
  extralines = list(
    "Mean Dep. Var."  = rep(mean_dep_var, 4),
    "N. Treated"      = rep(n_treated, 4),
    "N. Control"      = rep(n_control, 4),
    "T.E. (Prov-Year)" = c("No", "Yes", "No", "Yes"),
    "Period"          = rep("2011-2021", 4)
  ),
  headers = list("Extensive Margin" = 2, "Intensive Margin" = 2)
)

cat("Main regression coefficients:\n")
etable(m1, m2, m3, m4, keep_raw = "^treat$")
cat("✓ Main regression table saved\n")
