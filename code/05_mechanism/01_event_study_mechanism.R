## FxS 2026 — Event Studies: Mechanism Outcomes
## Original: 05_mechanism/figures/01_event_study_mechanism.R

rm(list = ls())
source(here::here("code/00_packages.R"))

run_event_study <- function(outcome_var, out_label) {

  df <- import(here("data/panel_grids_regressions.rds")) |>
    subset(year %in% 2011:2021 & sample_control_1 == 1) |>
    mutate(
      treated     = d_predio_in_grid,
      runtime     = case_when(treated == 1 ~ year - as.numeric(year_conf), .default = -1),
      interaction = interaction(runtime, treated),
      log_avg_nl  = log(nl_harm + 1),
      log_mapmuse = log(mapmuse + 1)
    ) |>
    rename(outcome = all_of(outcome_var)) |>
    select(id_grid, year, year_conf, provincia, outcome, treated, runtime, interaction)

  df <- dummy_cols(df, select_columns = "interaction", remove_selected_columns = TRUE) |>
    select(-ends_with(".0"), -"interaction_-1.1")
  names(df) <- gsub(".1",            "", names(df)) |>
               gsub("interaction_",  "interaction", x = _) |>
               gsub("interaction-",  "interaction_", x = _)
  names(df)[names(df) == "interaction"] <- "interaction1"

  modelo <- as.formula(paste0(
    "outcome ~ ",
    paste0("interaction_", abs(min(df$runtime)):2, collapse = "+"), "+",
    paste0("interaction",  0:abs(max(df$runtime)), collapse = "+"),
    " + as.factor(provincia):as.factor(year) | id_grid + year"
  ))

  reg <- feols(modelo, data = df, cluster = ~id_grid) |>
    tidy() |>
    subset(str_detect(term, "interaction")) |>
    select(term, estimate, std.error) |>
    mutate(
      term     = gsub("interaction", "", term) |> gsub("_", "-", x = _) |> as.numeric(),
      ci_lower = estimate - 1.96 * std.error,
      ci_upper = estimate + 1.96 * std.error
    )

  p <- ggplot(reg, aes(y = estimate, x = term)) +
    geom_hline(yintercept = 0,  colour = "gray60",  linetype = "solid",  size = 0.75) +
    geom_vline(xintercept = -1, colour = "red",     linetype = "dashed", size = 0.5)  +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = "CI 95"),
                  width = .05, show.legend = FALSE) +
    geom_point(aes(fill = "Coef"), shape = 21, size = 2, show.legend = FALSE) +
    scale_color_manual(values = c("CI 95" = "darkblue")) +
    scale_fill_manual(values  = c("Coef"  = "darkblue")) +
    scale_x_discrete(limits = unique(round(reg$term))) +
    theme_bw() +
    labs(y = "Effect Size", x = "Years to Land Titling",
         title = out_label)

  ggsave(here(paste0("output/figures/mechanisms/event_study_", outcome_var, ".png")),
         plot = p, width = 7, height = 4.5)
  cat("✓ Event study:", outcome_var, "\n")
}

## run for each mechanism outcome
run_event_study("log_avg_nl",   "Night Lights (log)")
run_event_study("erradi_h",     "Eradication (ha)")
run_event_study("loss_cover",   "Tree Cover Loss")
run_event_study("log_mapmuse",  "Drug Seizures (log)")
run_event_study("building",     "Built-Up Area")
