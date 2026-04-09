library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(ggpubr)

# Load and parse all sheets in one pass
path <- "./final_dataset/9final_results_Mar_7.xlsx"
sheet_names <- excel_sheets(path)
all_sheets <- lapply(sheet_names, function(x) {
  read_excel(path, sheet = x) %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%y"))
})
names(all_sheets) <- tolower(sheet_names)
combined_df <- bind_rows(all_sheets, .id = "sheet_origin")

# ============================================================
# Plot: Historical (<=2025) + Projections (2026+)
# ============================================================

sim_cols <- names(combined_df)[startsWith(names(combined_df), "sim")]
cutoff_date <- as.Date("2025-12-31")

# Fixed palette and display order
col_hist_sim      <- "#90A4AE"
col_best_fit      <- "#263238"
all_scenario_order <- c("Cov_60", "Cov_40", "cov_50_rela", "Cov_20")
scene_colors <- c(
  Cov_60      = "#FFB242FF",
  Cov_40      = "#DE4F33FF",
  cov_50_rela = "#9F2D55FF",
  Cov_20      = "#341648FF"
)

# State metadata: current vaccine coverage (numeric %)
states_to_plot  <- c("ca", "il", "ma", "ms", "tx")
current_cov_pct <- c(ca = 73.0, il = 72.3, ma = 94.0, ms = 57.8, tx = 72.9)

# Build per-state config: scenarios, labels, colors
state_config <- lapply(setNames(states_to_plot, states_to_plot), function(state) {
  cov      <- current_cov_pct[state]
  half_cov <- round(cov / 2, 1)

  # Exclude Cov_60 if baseline coverage is already below 60%
  scenarios <- all_scenario_order[all_scenario_order %in% c(
    if (cov >= 60) "Cov_60",
    "Cov_40", "cov_50_rela", "Cov_20"
  )]

  vax_labels <- c(
    Cov_60      = "Vaccine Coverage: 60%",
    Cov_40      = "Vaccine Coverage: 40%",
    cov_50_rela = paste0(
      "50% Reduction from Current Vaccine Coverage: ", half_cov, "%"
    ),
    Cov_20      = "Vaccine Coverage: 20%"
  )[scenarios]

  legend_order <- c("Simulations", "Best fit", scenarios)
  colors <- setNames(
    c(col_hist_sim, col_best_fit, unname(scene_colors[scenarios])),
    legend_order
  )
  legend_labels <- c(
    "Simulations" = "Simulation results",
    "Best fit"    = "Best fit",
    Cov_60        = "Coverage: 60%",
    Cov_40        = "Coverage: 40%",
    cov_50_rela   = paste0("50% rel. reduction: ", half_cov, "%"),
    Cov_20        = "Coverage: 20%"
  )[legend_order]

  list(
    scenarios     = scenarios,
    vax_labels    = vax_labels,
    legend_order  = legend_order,
    colors        = colors,
    legend_labels = legend_labels
  )
})

# Helper: stamp a df with panel + legend_type for each scenario
add_panels <- function(df, legend_label, scenarios) {
  bind_rows(lapply(scenarios, function(s) {
    df %>% mutate(panel = s, legend_type = legend_label)
  })) %>%
    mutate(panel = factor(panel, levels = scenarios))
}

# Function to build the plot for a given state
make_state_plot <- function(state_id) {
  state_df <- combined_df %>% filter(sheet_origin == state_id)

  hist_long <- state_df %>%
    select(Date, sheet_origin, all_of(sim_cols)) %>%
    pivot_longer(cols = all_of(sim_cols), names_to = "sim", values_to = "value")

  best_fit_long <- state_df %>%
    select(Date, sheet_origin, best_fit) %>%
    pivot_longer(cols = best_fit, names_to = "sim", values_to = "value")

  proj_long <- state_df %>%
    filter(Date > cutoff_date) %>%
    select(Date, sheet_origin, starts_with("cov")) %>%
    pivot_longer(cols = starts_with("cov"), names_to = "sim", values_to = "value") %>%
    filter(sim != "cov_94_scaled")

  cfg <- state_config[[state_id]]

  hist_long_panel <- add_panels(hist_long, "Simulations", cfg$scenarios)
  best_fit_panel  <- add_panels(best_fit_long, "Best fit", cfg$scenarios)
  proj_long <- proj_long %>%
    filter(sim %in% cfg$scenarios) %>%
    mutate(panel = factor(sim, levels = cfg$scenarios), legend_type = sim)

  ggplot() +
    geom_line(
      data = hist_long_panel,
      aes(x = Date, y = value,
          group = interaction(sim, sheet_origin),
          color = legend_type),
      alpha = 0.25, linewidth = 0.3
    ) +
    geom_line(
      data = best_fit_panel,
      aes(x = Date, y = value,
          group = interaction(sim, sheet_origin),
          color = legend_type),
      linewidth = 0.5
    ) +
    geom_line(
      data = proj_long,
      aes(x = Date, y = value, group = sim, color = legend_type),
      linewidth = 0.8
    ) +
    theme_pubr(base_size = 13) +
    scale_color_manual(
      values = cfg$colors,
      labels = cfg$legend_labels,
      breaks = cfg$legend_order,
      name   = "Legend"
    ) +
    guides(color = guide_legend(
      override.aes = list(
        alpha     = c(0.4, 1, rep(1, length(cfg$scenarios))),
        linewidth = c(0.5, 0.8, rep(0.8, length(cfg$scenarios)))
      )
    )) +
    geom_vline(
      xintercept = as.Date("2026-01-01"),
      linetype = "dashed", color = "gray50"
    ) +
    labs(
      title = toupper(state_id),
      x = "Year", y = "RVGE hospitalizations, Under 5"
    ) +
    facet_wrap(~ panel, ncol = 1, labeller = labeller(panel = cfg$vax_labels)) +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1),
      strip.text = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 15, face = "bold"),
      legend.text = element_text(size = 15),
      legend.key.size = unit(1.5, "cm"),
      panel.grid.minor = element_blank()
    ) +
    scale_x_date(
      limits = as.Date(c("2001-01-01", "2031-12-01")),
      date_breaks = "2 years", date_labels = "%Y"
    )
}

# Generate and save a plot for each state
for (state in states_to_plot) {
  p <- make_state_plot(state)
  ggsave(
    filename = paste0("./manuscript/0319_version/supplemental_figure_", state, ".png"),
    plot = p,
    width = 10, height = 16
  )
  message("Saved: supplemental_figure_", state, ".pdf")
}
