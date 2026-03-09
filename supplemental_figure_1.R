library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(ggpubr)
library(ragg)

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
usa_df <- combined_df %>% filter(sheet_origin == "usa")

# Data check: are all sim columns equal in historical rows?
hist_check <- usa_df %>%
  filter(Date <= cutoff_date) %>%
  select(Date, all_of(sim_cols))
hist_check$check <- ifelse(
  apply(hist_check[, sim_cols], 1, function(x) length(unique(x)) == 1),
  "PASS", "FAIL"
)
print(hist_check %>% select(Date, check))

# Data prep
hist_long <- usa_df %>%
  select(Date, sheet_origin, all_of(sim_cols)) %>%
  pivot_longer(cols = all_of(sim_cols), names_to = "sim", values_to = "value")

best_fit_long <- usa_df %>%
  select(Date, sheet_origin, best_fit) %>%
  pivot_longer(cols = best_fit, names_to = "sim", values_to = "value")

proj_long <- usa_df %>%
  filter(Date > cutoff_date) %>%
  select(Date, sheet_origin, starts_with("cov")) %>%
  pivot_longer(cols = starts_with("cov"), names_to = "sim", values_to = "value")

# Colors and labels
scenario_options <- c("Cov_60", "Cov_40", "Cov_20", "cov_94_scaled")
col_hist_sim <- "#90A4AE"
col_best_fit <- "#263238"

scene_colors <- setNames(
  c("#FFB242FF", "#DE4F33FF", "#341648FF", "#007BC3FF"),
  scenario_options
)

vax_labels <- c(
  "Cov_60"      = "Vaccine Coverage: 60%",
  "Cov_40"      = "Vaccine Coverage: 40%",
  "Cov_20"      = "Vaccine Coverage: 20%",
  # "cov_50_rela" = "50% Reduction from Current Vaccine Coverage",
  "cov_94_scaled" = "Vaccine Coverage: 94%"
)

# Helper: stamp a df with panel + legend_type for each scenario
add_panels <- function(df, legend_label) {
  bind_rows(lapply(scenario_options, function(s) {
    df %>% mutate(panel = s, legend_type = legend_label)
  }))
}

target_order <- c("Cov_60", "Cov_40", "cov_94_scaled", "Cov_20")

hist_long_panel <- add_panels(hist_long, "Simulations") %>%
  mutate(panel = factor(panel, levels = target_order))
best_fit_panel <- add_panels(best_fit_long, "Best fit") %>%
  mutate(panel = factor(panel, levels = target_order))
proj_long <- proj_long %>%
  mutate(panel = factor(sim, levels = target_order), legend_type = sim)

# Legend config
legend_order <- c("Simulations", "Best fit", scenario_options)
all_colors <- setNames(
  c(col_hist_sim, col_best_fit, unname(scene_colors)),
  legend_order
)
legend_labels <- c(
  "Simulations" = "Simulation results",
  "Best fit"           = "Best fit",
  "Cov_60"             = "Coverage: 60%",
  "Cov_40"             = "Coverage: 40%",
  "Cov_20"             = "Coverage: 20%",
  "cov_94_scaled"        = "Coverage: 94%"
)

# Filter to cov_94_scaled only
hist_94   <- hist_long_panel  %>% filter(panel == "cov_94_scaled")
best_94   <- best_fit_panel   %>% filter(panel == "cov_94_scaled")
proj_94   <- proj_long        %>% filter(panel == "cov_94_scaled")

legend_order_94 <- c("Simulations", "Best fit", "cov_94_scaled")
all_colors_94   <- all_colors[legend_order_94]
legend_labels_94 <- legend_labels[legend_order_94]

make_94_plot <- function(x_limits, x_breaks) {
  ggplot() +
    geom_line(
      data = hist_94,
      aes(x = Date, y = value,
          group = interaction(sim, sheet_origin),
          color = legend_type),
      alpha = 0.25, linewidth = 0.3
    ) +
    geom_line(
      data = best_94,
      aes(x = Date, y = value,
          group = interaction(sim, sheet_origin),
          color = legend_type),
      linewidth = 0.8
    ) +
    geom_line(
      data = proj_94,
      aes(x = Date, y = value, group = sim, color = legend_type),
      linewidth = 1
    ) +
    geom_vline(
      xintercept = as.Date("2026-01-01"),
      linetype = "dashed", color = "gray50"
    ) +
    theme_pubr(base_size = 13) +
    scale_color_manual(
      values = all_colors_94,
      labels = legend_labels_94,
      breaks = legend_order_94,
      name   = "Legend"
    ) +
    guides(color = guide_legend(
      override.aes = list(
        alpha     = c(0.4, 1, 1),
        linewidth = c(0.5, 0.8, 0.8)
      )
    )) +
    labs(
      x     = "Year",
      y     = "RVGE hospitalizations, Under 5",
      title = ""
    ) +
    theme(
      axis.text.x       = element_text(angle = 60, hjust = 1),
      strip.text        = element_text(size = 14, face = "bold"),
      legend.title      = element_text(size = 15, face = "bold"),
      legend.text       = element_text(size = 15),
      legend.key.size   = unit(1.5, "cm"),
      panel.grid.minor  = element_blank()
    ) +
    scale_x_date(
      limits      = as.Date(x_limits),
      date_breaks = x_breaks,
      date_labels = "%Y"
    )
}

# Full-range plot
proj_plot <- make_94_plot(
  x_limits = c("2001-01-01", "2035-12-01"),
  x_breaks = "2 years"
)

# Zoomed plot: 2023-07-01 to 2031-06-30
proj_plot_zoom <- make_94_plot(
  x_limits = c("2023-07-01", "2031-06-30"),
  x_breaks = "1 year"
) +
  coord_cartesian(ylim = c(0, 7500))

ragg::agg_png(
  "0309_cov94_full.png", width = 10, height = 6, units = "in", res = 300
)
print(proj_plot)
dev.off()

ragg::agg_png(
  "./figure_renders/0309_cov94_zoom.png", width = 10, height = 5, units = "in", res = 300
)
print(proj_plot_zoom)
dev.off()
