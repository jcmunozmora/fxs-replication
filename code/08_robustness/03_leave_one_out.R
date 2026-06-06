## FxS 2026 — Leave-One-Out Robustness: Doubly-Treated Grids
## Replicates Appendix Table: loo_doubly_treated
##
## Drops one doubly-treated grid at a time (FxS + Placa-Huella simultaneously).
## Shows psi (FxS x Placa-Huella interaction) is stable: mean -0.193, SD 0.017.

rm(list = ls())
source(here::here("code/00_packages.R"))

df <- import(here("data/panel_grids_regressions.rds")) |>
  subset(sample_control_1 == 1 & year %in% 2011:2021) |>
  mutate(
    treated      = treat_staggered,
    placa_huella = as.integer((d_vias_pdet + d_placa_huella) > 0),
    treated_ph   = treated * placa_huella,
    provincia_year = interaction(as.factor(provincia), as.factor(year))
  )

doubly_grids <- df |>
  subset(treated == 1 & placa_huella == 1) |>
  (\(x) unique(x$id_grid))()

cat(sprintf("Doubly-treated grids: %d\n", length(doubly_grids)))

run_model <- function(data) {
  feols(crops ~ treated + placa_huella + treated_ph | id_grid + year,
        data = data, vcov = ~id_grid)
}

baseline   <- run_model(df)
base_coef  <- broom::tidy(baseline) |> subset(term == "treated_ph")
cat(sprintf("Baseline psi: %.3f (SE=%.3f, p=%.3f)\n",
            base_coef$estimate, base_coef$std.error, base_coef$p.value))

## Leave-one-out loop
loo <- lapply(seq_along(doubly_grids), function(i) {
  fit <- tryCatch(
    run_model(df |> subset(id_grid != doubly_grids[i])),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  broom::tidy(fit) |>
    subset(term == "treated_ph") |>
    mutate(dropped_grid = doubly_grids[i],
           ci_lo = estimate - 1.96 * std.error,
           ci_hi = estimate + 1.96 * std.error)
})
loo_df <- do.call(rbind, Filter(Negate(is.null), loo))

cat(sprintf("\nLOO summary: n=%d, mean=%.3f, SD=%.3f, min=%.3f, max=%.3f, sig(p<.05)=%d\n",
            nrow(loo_df), mean(loo_df$estimate), sd(loo_df$estimate),
            min(loo_df$estimate), max(loo_df$estimate),
            sum(loo_df$p.value < 0.05)))

## ── Plot ─────────────────────────────────────────────────────────────────────
ggplot(loo_df, aes(x = reorder(factor(dropped_grid), estimate),
                   y = estimate)) +
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +
  geom_hline(yintercept = base_coef$estimate,
             color = "firebrick", linetype = "dotted", linewidth = 0.8) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                width = 0.25, alpha = 0.8, color = "steelblue4") +
  geom_point(size = 2.5, color = "steelblue4") +
  annotate("text", x = 1, y = base_coef$estimate + 0.015,
           label = "Baseline", color = "firebrick", size = 3, hjust = 0) +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        panel.grid.minor = element_blank()) +
  labs(x     = "Grid dropped",
       y     = expression(hat(psi) ~ "(FxS" %*% "Placa-Huella)"),
       title = "Leave-one-out: doubly-treated grids",
       caption = "Red line = baseline estimate. Bars = 95% CI.")

ggsave(here("output/figures/robustness/loo_doubly_treated.png"),
       width = 9, height = 5, dpi = 200)

## ── Table ────────────────────────────────────────────────────────────────────
loo_out <- loo_df |>
  mutate(Signif = dplyr::case_when(
    p.value < 0.01 ~ "***", p.value < 0.05 ~ "**",
    p.value < 0.10 ~ "*",  TRUE            ~ ""
  )) |>
  dplyr::select(Grid = dropped_grid, Estimate = estimate,
                SE = std.error, p_value = p.value,
                CI_lo = ci_lo, CI_hi = ci_hi, Signif) |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(., 3)))

write.csv(loo_out, here("output/tables/robustness/loo_doubly_treated.csv"),
          row.names = FALSE)
cat("✓ LOO table and figure saved\n")
