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
  pivot_longer(cols = starts_with("cov"), names_to = "sim", values_to = "value")%>%
  filter(sim != "cov_94_scaled")

# Colors and labels
scenario_options <- c("Cov_60", "Cov_40", "Cov_20", "cov_50_rela")
col_hist_sim <- "#90A4AE"
col_best_fit <- "#263238"

scene_colors <- setNames(
  c("#FFB242FF", "#DE4F33FF", "#341648FF", "#9F2D55FF"),
  scenario_options
)

vax_labels <- c(
  "Cov_60"      = "Vaccine Coverage: 60%",
  "Cov_40"      = "Vaccine Coverage: 40%",
  "Cov_20"      = "Vaccine Coverage: 20%",
  "cov_50_rela" = "50% Reduction from Current Vaccine Coverage: 37.7%"
)

# Helper: stamp a df with panel + legend_type for each scenario
add_panels <- function(df, legend_label) {
  bind_rows(lapply(scenario_options, function(s) {
    df %>% mutate(panel = s, legend_type = legend_label)
  }))
}

target_order <- c("Cov_60", "Cov_40", "cov_50_rela", "Cov_20")

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
  "cov_50_rela"        = "50% rel. reduction: 37.7%"
)

proj_plot <- ggplot() +
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
    values = all_colors,
    labels = legend_labels,
    breaks = legend_order,
    name   = "Legend"
  ) +
  guides(color = guide_legend(
    override.aes = list(
      alpha     = c(0.4, 1, 1, 1, 1, 1),
      linewidth = c(0.5, 0.8, 0.8, 0.8, 0.8, 0.8)
    )
  )) +
  geom_vline(
    xintercept = as.Date("2026-01-01"),
    linetype = "dashed", color = "gray50"
  ) +
  labs(x = "Year", y = "RVGE hospitalizations, Under 5") +
  facet_wrap(~ panel, ncol = 1, labeller = labeller(panel = vax_labels)) +
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

ggsave(
  plot = proj_plot,
  filename = "./figure_renders/0309_projection_plots.jpg",
  device = "jpeg", width = 10, height = 12, dpi = 200
)
