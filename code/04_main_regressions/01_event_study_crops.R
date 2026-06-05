## FxS 2026 — Event Study: Main Outcome (Coca Crops)
## Original: 04_main_regressions/figures/01_event_study_crops.R

rm(list = ls())
source(here::here("code/00_packages.R"))

df <- import(here("data/panel_grids_regressions.rds")) |>
  subset(year %in% 2011:2021 & sample_control_1 == 1) |>
  mutate(
    treated    = d_predio_in_grid,
    runtime    = case_when(treated == 1 ~ year - as.numeric(year_conf), .default = -1),
    interaction = interaction(runtime, treated)
  ) |>
  select(id_grid, year, year_conf, provincia, crops, treated, runtime, interaction)

df <- dummy_cols(df, select_columns = "interaction", remove_selected_columns = TRUE)
df <- df |>
  select(-ends_with(".0"), -"interaction_-1.1")
names(df) <- gsub(".1",       "", names(df))  |>
             gsub("interaction_", "interaction", x = _) |>
             gsub("interaction-", "interaction_", x = _)
names(df)[names(df) == "interaction"] <- "interaction1"

modelo <- as.formula(paste0(
  "crops ~ ",
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

ggplot(reg, aes(y = estimate, x = term)) +
  geom_hline(yintercept = 0,  colour = "gray60",  linetype = "solid",  size = 0.75) +
  geom_vline(xintercept = -1, colour = "red",     linetype = "dashed", size = 0.5)  +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = "CI 95"),
                width = .05, show.legend = FALSE) +
  geom_point(aes(fill = "Coef"), shape = 21, size = 2, show.legend = FALSE) +
  scale_color_manual(values = c("CI 95" = "darkblue")) +
  scale_fill_manual(values  = c("Coef"  = "darkblue")) +
  scale_x_discrete(limits = unique(round(reg$term))) +
  theme_bw() +
  theme(
    axis.text.x  = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 13),
    axis.text.y  = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 13)
  ) +
  labs(y = "Effect Size", x = "Years to Land Titling")

ggsave(here("output/figures/main-regressions/event_study_crops.png"), width = 7, height = 4.5)
cat("✓ Event study (crops) saved\n")
