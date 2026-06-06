## FxS 2026 — Conley (1999) Spatial Standard Errors
## Replicates Appendix Table: conley_se_comparison
##
## Requires: data/grid_centroids.csv (anonymized grid centroids, WGS84)
## Cutoffs: 1 km, 5 km, 10 km.
## Key result: significant at 5% through 5km; 10% at 10km (likely overcorrects).

rm(list = ls())
source(here::here("code/00_packages.R"))

df <- import(here("data/panel_grids_regressions.rds")) |>
  subset(sample_control_1 == 1 & year %in% 2011:2021) |>
  mutate(treated        = treat_staggered,
         provincia_year = interaction(as.factor(provincia), as.factor(year)))

## Merge anonymized centroids
centroids <- read.csv(here("data/grid_centroids.csv"))
df <- left_join(df, centroids, by = "id_grid")
cat(sprintf("Grids with centroids: %d / %d\n",
            sum(!is.na(df$lat)), nrow(df)))

## ── Models ───────────────────────────────────────────────────────────────────
m_cl   <- feols(crops ~ treated | id_grid + year, data = df, vcov = ~id_grid)
m_c1   <- feols(crops ~ treated | id_grid + year, data = df, vcov = conley(1))
m_c5   <- feols(crops ~ treated | id_grid + year, data = df, vcov = conley(5))
m_c10  <- feols(crops ~ treated | id_grid + year, data = df, vcov = conley(10))
m_cl_p <- feols(crops ~ treated | id_grid + year + provincia_year,
                data = df, vcov = ~id_grid)
m_c1_p <- feols(crops ~ treated | id_grid + year + provincia_year,
                data = df, vcov = conley(1))
m_c5_p <- feols(crops ~ treated | id_grid + year + provincia_year,
                data = df, vcov = conley(5))
m_c10p <- feols(crops ~ treated | id_grid + year + provincia_year,
                data = df, vcov = conley(10))

etable(m_cl, m_c1, m_c5, m_c10,
       m_cl_p, m_c1_p, m_c5_p, m_c10p,
       keep    = "treated",
       headers = c("Cluster", "1 km", "5 km", "10 km",
                   "Cluster+PY", "1km+PY", "5km+PY", "10km+PY"),
       title   = "Main result under Conley (1999) spatial standard errors",
       label   = "tab:conley_se",
       notes   = paste0(
         "Centroids extracted from UNODC grid shapefile (WGS84). ",
         "The 5~km cutoff (five grid-rings) is the benchmark; ",
         "the 10~km cutoff likely overcorrects. ",
         "$^{***}$~$p{<}0.01$, $^{**}$~$p{<}0.05$, $^{*}$~$p{<}0.1$."
       ),
       tex  = TRUE,
       file = here("output/tables/robustness/conley_se_comparison.tex"))

cat("✓ Conley SE table saved\n")
