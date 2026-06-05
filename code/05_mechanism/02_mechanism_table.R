## FxS 2026 — Mechanism DiD Table (Economic Activity & Investment)
## Original: 05_mechanism/tables/03_tbl_dif-dif-mechanism.R

rm(list = ls())
source(here::here("code/00_packages.R"))

df <- import(here("data/panel_grids_regressions.rds")) |>
  mutate(
    treat       = treat_staggered,
    log_avg_nl  = log(nl_harm + 1),
    loss_cover  = p2000_loss_cover
  )

## ── sample stats ─────────────────────────────────────────────────────────────
get_mean <- function(var, year_ini = 2011, year_end = 2021) {
  df |> subset(year %in% year_ini:year_end & sample_control_1 == 1 &
                 d_control_1 == 1 & treat_staggered == 0) |>
    pull({{ var }}) |> mean(na.rm = TRUE) |> round(3)
}

n_control <- df |> subset(sample_control_1 == 1 & d_control_1 == 1) |>
               distinct(id_grid) |> nrow()
n_treated <- df |> subset(sample_control_1 == 1 & d_treated_1 == 1) |>
               distinct(id_grid) |> nrow()

mean_nl   <- get_mean(log_avg_nl)
mean_loss <- get_mean(loss_cover)
mean_bld  <- get_mean(building)

## ── estimation ───────────────────────────────────────────────────────────────
samp <- df |> subset(sample_control_1 == 1 & year %in% 2011:2021)

m1 <- feols(log_avg_nl ~ treat | id_grid + year, data = samp, cluster = ~id_grid)
m2 <- feols(log_avg_nl ~ treat + as.factor(provincia):as.factor(year) | id_grid + year,
            data = samp, cluster = ~id_grid)
m3 <- feols(loss_cover ~ treat | id_grid + year,  data = samp, cluster = ~id_grid)
m4 <- feols(loss_cover ~ treat + as.factor(provincia):as.factor(year) | id_grid + year,
            data = samp, cluster = ~id_grid)
m5 <- feols(building   ~ treat | id_grid + year,  data = samp, cluster = ~id_grid)
m6 <- feols(building   ~ treat + as.factor(provincia):as.factor(year) | id_grid + year,
            data = samp, cluster = ~id_grid)

## ── table ─────────────────────────────────────────────────────────────────────
etable(
  m1, m2, m3, m4, m5, m6,
  keep_raw = "^treat$",
  dict     = c("treat" = "\\text{FxS}"),
  tex      = TRUE,
  file     = here("output/tables/mechanisms/economic_activity_2011_2021.tex"),
  digits   = 3,
  se.below = TRUE,
  fitstat  = ~ n,
  extralines = list(
    "Mean Dep. Var."   = c(rep(mean_nl, 2), rep(mean_loss, 2), rep(mean_bld, 2)),
    "N. Treated"       = rep(n_treated, 6),
    "N. Control"       = rep(n_control, 6),
    "T.E. (Prov-Year)" = c("No","Yes","No","Yes","No","Yes"),
    "Period"           = rep("2011-2021", 6)
  ),
  headers = list("Night Lights (log)" = 2, "Deforestation" = 2, "Built-Up Area" = 2)
)

cat("Mechanism coefficients:\n")
etable(m1, m2, m3, m4, m5, m6, keep_raw = "^treat$")
cat("✓ Mechanism table saved\n")
