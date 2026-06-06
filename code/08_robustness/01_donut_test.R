## FxS 2026 — Donut Test: Second-Ring Neighbor Control Group
## Replicates Appendix Table: donut_test
##
## Tests for SUTVA violation (coca displacement to first-ring control grids).
## If Ring2 coefficient is similar to or larger than Ring1, no displacement.
## Result: Ring2 = -0.527 (Ring1 baseline = -0.383); displacement ruled out.

rm(list = ls())
source(here::here("code/00_packages.R"))

df_full <- import(here("data/panel_grids_regressions.rds"))

## ── Sample definitions ────────────────────────────────────────────────────────
df_ring1 <- df_full |>
  subset(sample_control_1 == 1 & year %in% 2011:2021) |>
  mutate(
    treated        = treat_staggered,
    provincia_year = interaction(as.factor(provincia), as.factor(year))
  )

## Second-ring neighbors: d_grid_nb_2 == 1 (adjacent to ring1 but NOT to treated)
df_donut <- df_full |>
  subset((d_treated_1 == 1 & sample_control_1 == 1) |
           d_grid_nb_2 == 1) |>
  subset(year %in% 2011:2021) |>
  mutate(
    treated        = treat_staggered,
    provincia_year = interaction(as.factor(provincia), as.factor(year))
  )

cat(sprintf("Ring1: %d obs | %d treated | %d control\n",
            nrow(df_ring1),
            length(unique(df_ring1$id_grid[df_ring1$d_treated_1 == 1])),
            length(unique(df_ring1$id_grid[df_ring1$d_treated_1 == 0]))))
cat(sprintf("Ring2 (donut): %d obs | %d treated | %d ring2\n",
            nrow(df_donut),
            length(unique(df_donut$id_grid[df_donut$d_treated_1 == 1])),
            length(unique(df_donut$id_grid[df_donut$d_grid_nb_2 == 1]))))

## ── Regressions ──────────────────────────────────────────────────────────────
m_r1  <- feols(crops ~ treated | id_grid + year,
               data = df_ring1, vcov = ~id_grid)
m_r1p <- feols(crops ~ treated | id_grid + year + provincia_year,
               data = df_ring1, vcov = ~id_grid)
m_d   <- feols(crops ~ treated | id_grid + year,
               data = df_donut, vcov = ~id_grid)
m_dp  <- feols(crops ~ treated | id_grid + year + provincia_year,
               data = df_donut, vcov = ~id_grid)

etable(m_r1, m_r1p, m_d, m_dp,
       keep    = "treated",
       headers = c("Ring1", "Ring1+PY", "Ring2 (donut)", "Ring2+PY"),
       title   = "Donut test: second-ring neighbor control group",
       label   = "tab:donut_test",
       notes   = paste0(
         "Ring1 = immediate neighbors (sample\\_control\\_1 == 1, N=526). ",
         "Ring2 = second-ring neighbors (d\\_grid\\_nb\\_2 == 1, N=1{,}103): ",
         "adjacent to Ring1 but not adjacent to treated grids. ",
         "Displacement would produce a Ring2 coefficient closer to zero; ",
         "the larger Ring2 estimate rules out systematic displacement. ",
         "$^{***}$~$p{<}0.01$, $^{**}$~$p{<}0.05$, $^{*}$~$p{<}0.1$."
       ),
       tex  = TRUE,
       file = here("output/tables/robustness/donut_test.tex"))

cat("✓ Donut test table saved\n")

## ── Event study (donut sample) ───────────────────────────────────────────────
make_es <- function(df) {
  df |>
    mutate(
      yconf_int = suppressWarnings(as.integer(as.character(year_conf))),
      yref      = if_else(d_treated_1 == 1L, yconf_int, 2018L),
      rel_time  = pmax(pmin(as.integer(year) - yref, 3L), -7L),
      rel_time_f = relevel(factor(rel_time), ref = "-1")
    )
}

es_r1 <- feols(crops ~ i(rel_time, ref = -1) | id_grid + year,
               data = make_es(df_ring1), vcov = ~id_grid)
es_d  <- feols(crops ~ i(rel_time, ref = -1) | id_grid + year,
               data = make_es(df_donut), vcov = ~id_grid)

tidy_es <- function(m, label) {
  broom::tidy(m, conf.int = TRUE) |>
    mutate(t = as.integer(gsub("rel_time::", "", term)),
           Control = label) |>
    bind_rows(tibble(t = -1L, estimate = 0, conf.low = 0,
                     conf.high = 0, Control = label))
}

es_df <- bind_rows(tidy_es(es_r1, "Ring1 (baseline)"),
                   tidy_es(es_d,  "Ring2 (donut)")) |>
  filter(!is.na(t))

ggplot(es_df, aes(x = t, y = estimate, color = Control, fill = Control)) +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_vline(xintercept = -0.5, color = "firebrick",
             linetype = "dashed", linewidth = 0.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2) +
  scale_color_manual(values = c("steelblue4", "darkorange3")) +
  scale_fill_manual(values  = c("steelblue4", "darkorange3")) +
  scale_x_continuous(breaks = -7:3) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        legend.position  = "bottom") +
  labs(x       = "Years relative to FxS title delivery",
       y       = "Coca area (ha / 100-ha grid)",
       title   = "Donut test: event study",
       caption = "Ring1 = immediate neighbors (N=526). Ring2 = second-ring (N=1,103). 95% CI.",
       color = "Control group", fill = "Control group")

ggsave(here("output/figures/robustness/donut_test_event_study.png"),
       width = 9, height = 5, dpi = 200)
cat("✓ Donut event study figure saved\n")
