## FxS 2026 — Roads: Interaction with Coca Crops
## Original: 06_roads/tables/01_roads_placa_huella_crops.R

rm(list = ls())
source(here::here("code/00_packages.R"))

df <- import(here("data/panel_grids_regressions.rds")) |>
  subset(sample_control_1 == 1 & year %in% 2011:2021) |>
  mutate(
    treated              = treat_staggered,
    roads                = ifelse(roads_rural > 0, 1, 0),
    placa_huella         = ifelse(d_vias_pdet + d_placa_huella > 0, 1, 0),
    public_goods         = ifelse(d_obras_pdet + d_material + d_maquinaria +
                                  d_proyectos + d_proyectos_agricola > 0, 1, 0),
    treated_roads        = treated * roads,
    treated_placa_huella = treated * placa_huella,
    treated_public_goods = treated * public_goods
  )

n_control    <- df |> subset(d_control_1 == 1) |> distinct(id_grid) |> nrow()
n_treated    <- df |> subset(d_treated_1 == 1) |> distinct(id_grid) |> nrow()
mean_dep_var <- df |> subset(d_control_1 == 1 & treat_staggered == 0) |>
                  pull(crops) |> mean(na.rm = TRUE) |> round(3)

## ── regressions ───────────────────────────────────────────────────────────────
m1 <- feols(crops ~ treated + roads + treated_roads | id_grid + year,
            data = df, cluster = ~id_grid)
m2 <- feols(crops ~ treated + roads + treated_roads +
              as.factor(provincia):as.factor(year) | id_grid + year,
            data = df, cluster = ~id_grid)
m3 <- feols(crops ~ treated + placa_huella + treated_placa_huella | id_grid + year,
            data = df, cluster = ~id_grid)
m4 <- feols(crops ~ treated + placa_huella + treated_placa_huella +
              as.factor(provincia):as.factor(year) | id_grid + year,
            data = df, cluster = ~id_grid)
m5 <- feols(crops ~ treated + public_goods + treated_public_goods | id_grid + year,
            data = df, cluster = ~id_grid)
m6 <- feols(crops ~ treated + public_goods + treated_public_goods +
              as.factor(provincia):as.factor(year) | id_grid + year,
            data = df, cluster = ~id_grid)

etable(
  m1, m2, m3, m4, m5, m6,
  keep_raw = c("^treated$", "^treated_roads$", "^treated_placa_huella$", "^treated_public_goods$"),
  dict     = c("treated" = "\\text{FxS}",
               "treated_roads"        = "\\text{FxS$*$Rural-Roads}",
               "treated_placa_huella" = "\\text{FxS$*$Placa-Huella}",
               "treated_public_goods" = "\\text{FxS$*$Public-Goods}"),
  tex      = TRUE,
  file     = here("output/tables/roads/roads_placa_huella_crops_2011_2021.tex"),
  digits   = 3,
  se.below = TRUE,
  fitstat  = ~ n,
  extralines = list(
    "Mean Dep. Var."   = rep(mean_dep_var, 6),
    "N. Treated"       = rep(n_treated, 6),
    "N. Control"       = rep(n_control, 6),
    "T.E. (Prov-Year)" = c("No","Yes","No","Yes","No","Yes"),
    "Period"           = rep("2011-2021", 6)
  ),
  headers = list("Rural Roads" = 2, "Placa Huella" = 2, "Public Goods" = 2)
)

cat("Roads × Crops coefficients:\n")
etable(m1, m2, m3, m4, m5, m6,
       keep_raw = c("^treated$","^treated_roads$","^treated_placa_huella$","^treated_public_goods$"))
cat("✓ Roads main table saved\n")
