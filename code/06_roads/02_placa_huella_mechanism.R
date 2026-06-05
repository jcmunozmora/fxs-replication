## FxS 2026 — Placa Huella × Economic Activity & Enforcement
## Original: 06_roads/tables/02_placa_huella_mechanism.R

rm(list = ls())
source(here::here("code/00_packages.R"))

df <- import(here("data/panel_grids_regressions.rds")) |>
  subset(sample_control_1 == 1 & year %in% 2011:2021) |>
  mutate(
    treated              = treat_staggered,
    placa_huella         = ifelse(d_vias_pdet + d_placa_huella > 0, 1, 0),
    treated_placa_huella = treated * placa_huella,
    log_avg_nl           = log(nl_harm + 1),
    loss_cover           = p2000_loss_cover,
    log_mapmuse          = log(mapmuse + 1)
  )

n_control <- df |> subset(d_control_1 == 1) |> distinct(id_grid) |> nrow()
n_treated <- df |> subset(d_treated_1 == 1) |> distinct(id_grid) |> nrow()

## means
mean_nl    <- df |> subset(d_control_1 == 1 & treat_staggered == 0) |> pull(log_avg_nl)   |> mean(na.rm=T) |> round(3)
mean_lc    <- df |> subset(d_control_1 == 1 & treat_staggered == 0) |> pull(loss_cover)   |> mean(na.rm=T) |> round(3)
mean_bld   <- df |> subset(year >= 2016 & d_control_1 == 1 & treat_staggered == 0) |> pull(building) |> mean(na.rm=T) |> round(3)
mean_muse  <- df |> subset(d_control_1 == 1 & treat_staggered == 0) |> pull(log_mapmuse) |> mean(na.rm=T) |> round(3)
mean_erradi <- df |> subset(d_control_1 == 1 & treat_staggered == 0) |> pull(erradi_h)    |> mean(na.rm=T) |> round(3)

## ── Economic Activity ─────────────────────────────────────────────────────────
a1 <- feols(log_avg_nl  ~ treated + treated_placa_huella | id_grid + year, data = df, cluster = ~id_grid)
a2 <- feols(log_avg_nl  ~ treated + treated_placa_huella + as.factor(provincia):as.factor(year) | id_grid + year, data = df, cluster = ~id_grid)
a3 <- feols(loss_cover  ~ treated + treated_placa_huella | id_grid + year, data = df, cluster = ~id_grid)
a4 <- feols(loss_cover  ~ treated + treated_placa_huella + as.factor(provincia):as.factor(year) | id_grid + year, data = df, cluster = ~id_grid)
a5 <- feols(building    ~ treated + treated_placa_huella | id_grid + year, data = df, cluster = ~id_grid)
a6 <- feols(building    ~ treated + treated_placa_huella + as.factor(provincia):as.factor(year) | id_grid + year, data = df, cluster = ~id_grid)

etable(a1, a2, a3, a4, a5, a6,
       keep_raw = c("^treated$","^treated_placa_huella$"),
       dict     = c("treated" = "\\text{FxS}", "treated_placa_huella" = "\\text{FxS$*$Placa-Huella}"),
       tex      = TRUE,
       file     = here("output/tables/roads/placa_huella_economic_activity_2011_2021.tex"),
       digits   = 3, se.below = TRUE, fitstat = ~ n,
       extralines = list(
         "Mean Dep. Var."   = c(rep(mean_nl,2), rep(mean_lc,2), rep(mean_bld,2)),
         "N. Treated"       = rep(n_treated, 6),
         "N. Control"       = rep(n_control, 6),
         "T.E. (Prov-Year)" = c("No","Yes","No","Yes","No","Yes"),
         "Period"           = rep("2011-2021", 6)
       ),
       headers = list("Night Lights" = 2, "Deforestation" = 2, "Building" = 2))

## ── Enforcement ───────────────────────────────────────────────────────────────
b1 <- feols(log_mapmuse ~ treated + treated_placa_huella | id_grid + year, data = df, cluster = ~id_grid)
b2 <- feols(log_mapmuse ~ treated + treated_placa_huella + as.factor(provincia):as.factor(year) | id_grid + year, data = df, cluster = ~id_grid)
b3 <- feols(erradi_h    ~ treated + treated_placa_huella | id_grid + year, data = df, cluster = ~id_grid)
b4 <- feols(erradi_h    ~ treated + treated_placa_huella + as.factor(provincia):as.factor(year) | id_grid + year, data = df, cluster = ~id_grid)

etable(b1, b2, b3, b4,
       keep_raw = c("^treated$","^treated_placa_huella$"),
       dict     = c("treated" = "\\text{FxS}", "treated_placa_huella" = "\\text{FxS$*$Placa-Huella}"),
       tex      = TRUE,
       file     = here("output/tables/roads/placa_huella_enforcement_2011_2021.tex"),
       digits   = 3, se.below = TRUE, fitstat = ~ n,
       extralines = list(
         "Mean Dep. Var."   = c(rep(mean_muse, 2), rep(mean_erradi, 2)),
         "N. Treated"       = rep(n_treated, 4),
         "N. Control"       = rep(n_control, 4),
         "T.E. (Prov-Year)" = c("No","Yes","No","Yes"),
         "Period"           = rep("2011-2021", 4)
       ),
       headers = list("Victims of Landmines" = 2, "Eradication" = 2))

cat("Placa Huella mechanism coefficients:\n")
etable(a1, a2, a3, a4, a5, a6, keep_raw = c("^treated$","^treated_placa_huella$"))
etable(b1, b2, b3, b4, keep_raw = c("^treated$","^treated_placa_huella$"))
cat("✓ Placa Huella mechanism tables saved\n")
