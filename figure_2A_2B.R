library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(purrr)
library(maps)
library(ggrepel)
library(patchwork)
library(ggnewscale)
library(ragg)
library(forcats)
library(usdata)
# Load and parse all sheets in one pass
path <- "./final_dataset/9final_results_Mar_7.xlsx"
sheet_names <- excel_sheets(path)
all_sheets <- lapply(sheet_names, function(x) {
  read_excel(path, sheet = x) %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%y"))
})
names(all_sheets) <- tolower(sheet_names)
combined_df <- bind_rows(all_sheets, .id = "sheet_origin")

# Load population data (unchanged from rota_6)
pop <- "./final_dataset/9pop_results_Mar_7.xlsx"
pop_sheets <- excel_sheets(pop)
all_pop <- lapply(pop_sheets, function(x) read_excel(pop, sheet = x))
names(all_pop) <- pop_sheets
pop_df <- bind_rows(all_pop, .id = "sheet_origin")

state_dat <- combined_df %>% filter(sheet_origin != "usa")

# ============================================================
# State-level summary function
# ============================================================

states_to_plot <- c(
  "California", "Illinois", "Massachusetts", "Mississippi", "Texas"
)
current_cov <- c("73%", "72.3%", "94%", "57.8%", "78.9%")
proj_cols <- c("Cov_60", "Cov_40", "Cov_20", "cov_50_rela")

# sheet_origin in the new xlsx uses 2-letter abbreviations
state_abbrev <- c(
  "California"   = "ca",
  "Illinois"     = "il",
  "Massachusetts" = "ma",
  "Mississippi"  = "ms",
  "Texas"        = "tx"
)

cases_state <- function(data, state, coverage, start_year, proj_end) {
  cov_num <- as.numeric(gsub("%", "", coverage))
  abbrev <- state_abbrev[state]

  # Historical: best_fit Jul 2023 – Jun 2025
  hist_df <- data %>%
    filter(
      Date >= as.Date(paste0(start_year, "-07-01")) &
        Date < as.Date("2025-07-01"),
      sheet_origin == abbrev
    ) %>%
    select(Date, sheet_origin, RVGE = best_fit)

  hist_sum <- sum(hist_df$RVGE, na.rm = TRUE)

  # Projections: Jul 2027 – Jun 2029
  proj_results <- data %>%
    filter(
      Date >= as.Date("2027-07-01") &
        Date < as.Date(paste0(proj_end, "-07-01")),
      sheet_origin == abbrev
    ) %>%
    select(Date, all_of(proj_cols)) %>%
    pivot_longer(cols = -Date, names_to = "Scenario", values_to = "RVGE") %>%
    mutate(Scenario_Value = case_when(
      Scenario == "cov_50_rela" ~ cov_num / 2,
      Scenario == "Cov_60"     ~ 60,
      Scenario == "Cov_40"     ~ 40,
      Scenario == "Cov_20"     ~ 20
    )) %>%
    filter(Scenario_Value <= cov_num) %>%
    group_by(Scenario) %>%
    summarise(projections = sum(RVGE, na.rm = TRUE), .groups = "drop") %>%
    mutate(state = state, past = hist_sum) %>%
    select(state, Scenario, past, projections)

  proj_results
}

all_results <- map2_dfr(states_to_plot, current_cov, function(st, cov) {
  cases_state(
    data = state_dat,
    state = st,
    coverage = cov,
    start_year = "2023",
    proj_end = "2029"
  )
})

# Map uses the 50% relative reduction scenario
map_data_subset <- all_results %>%
  filter(Scenario == "cov_50_rela")

# ============================================================
# Map: US states colored by current coverage
# ============================================================

coverage_df <- data.frame(
  region = tolower(states_to_plot),
  cov_pct = as.numeric(gsub("%", "", current_cov))
)

us_states <- map_data("state")
us_states <- left_join(us_states, coverage_df)

df <- data.frame(
  state = c("California", "Illinois", "Massachusetts", "Mississippi", "Texas"),
  lat = c(36.77, 39.73, 42.40, 32.74, 31.05),
  lon = c(-119.41, -89.49, -71.38, -89.67, -97.56),
  current_cov = c("73%", "72.3%", "94%", "57.8%", "72.9%")
)

map_data_subset <- left_join(map_data_subset, df, by = "state")

# Population for incidence (historical 3 yrs: 2023-26, projections 5 yrs: 2026-31)
# states_pop <- c("california", "Illinois", "Massachusetts", "mississippi", "texas")
# 
# pop_df_long <- pop_df %>%
#   dplyr::filter(YEAR %in% c("2024", "2028")) %>%
#   tidyr::pivot_longer(
#     cols = all_of(states_pop),
#     names_to = "state_name",
#     values_to = "U5_pop"
#   ) %>%
#   dplyr::mutate(
#     joinkey = tolower(state_name),
#     state_clean = str_to_title(joinkey)
#   ) %>%
#   dplyr::select(state_clean, joinkey, YEAR, U5_pop)
# 
# pop_df_wide <- pop_df_long %>%
#   pivot_wider(
#     names_from = YEAR,
#     values_from = U5_pop,
#     names_prefix = "U5_pop_"
#   ) %>%
#   select(state_clean, U5_pop_2024, U5_pop_2028, joinkey)

pop_df_wide<- pop_df %>% 
  select(-c(sheet_origin, USA)) %>%
  filter(year %in% c("2023-2024", "2024-2025", "2027-2028", "2028-2029"))%>% 
  pivot_longer(cols = CA:MS) %>% 
  pivot_wider(names_from = year, values_from = value, names_prefix = "U5_pop_") %>%
  mutate(joinkey = tolower(abbr2state(name)))

map_data_subset <- map_data_subset %>%
  mutate(joinkey = tolower(state))
map_data_subset <- left_join(map_data_subset, pop_df_wide, by = "joinkey")
# both windows = 2 years → /2 for per-year rate per 10,000
# map_data_subset$incidence_past <-
#   (map_data_subset$past / map_data_subset$U5_pop_2024) * 10000 / 2
# map_data_subset$incidence_proj <-
#   (map_data_subset$projections / map_data_subset$U5_pop_2028) * 10000 / 2

map_data_subset$incidence_past <-
  (map_data_subset$past / (map_data_subset$`U5_pop_2023-2024` + map_data_subset$`U5_pop_2024-2025`)) * 10000
map_data_subset$incidence_proj <-
  (map_data_subset$projections / (map_data_subset$`U5_pop_2027-2028` + map_data_subset$`U5_pop_2028-2029`)) * 10000

max_val <- max(
  map_data_subset$incidence_past,
  na.rm = TRUE
)
min_val <- min(
  map_data_subset$incidence_past,
  na.rm = TRUE
)

p1 <- ggplot() +
  geom_polygon(
    data = us_states,
    aes(x = long, y = lat, group = group, fill = cov_pct),
    color = "white"
  ) +
  scale_fill_gradient(
    low = "#EAF3FFFF", high = "#08519CFF",
    na.value = "#F0F0F0FF", name = "Current Coverage %",
    guide = guide_colorbar(
      barwidth = 15, barheight = 0.5,
      title.position = "left", title.hjust = 0.5
    )
  ) +
  new_scale_fill() +
  geom_point(
    data = map_data_subset,
    aes(x = lon, y = lat, size = incidence_past, fill = incidence_past),
    color = "white", stroke = 0.5, shape = 21, alpha = 0.9
  ) +
  geom_text_repel(
    data = map_data_subset,
    aes(x = lon - 1, y = lat + 1, label = state),
    fontface = "bold", size = 3.5
  ) +
  scale_size_continuous(
    range = c(3, 18), limits = c(min_val, max_val), guide = "none"
  ) +
  scale_fill_gradientn(
    colours = c(
      "#FCDE9CFF", "#FAA476FF", "#F0746EFF",
      "#E34F6FFF", "#DC3977FF", "#B9257AFF", "#980043FF"
    ),
    limits = c(min_val, max_val),
    name = "Incidence (per 10,000)",
    guide = guide_colorbar(
      barwidth = 15, barheight = 0.5,
      title.position = "left", title.hjust = 0.5
    )
  ) +
  theme_void() +
  theme(legend.position = "bottom")

# ggsave(
#   plot = p1,
#   filename = "./figure_renders/0302_state_level_map.png",
#   device = agg_png, width = 12, height = 8, units = "in", dpi = 320
# )

# ============================================================
# Dot plot: state-level incidence by scenario
# ============================================================

all_results <- all_results %>%
  mutate(joinkey = tolower(state))

all_results_dot <- left_join(all_results, pop_df_wide, by = "joinkey")
all_results_dot$incidence_past <-
  (all_results_dot$past / (all_results_dot$`U5_pop_2023-2024` + all_results_dot$`U5_pop_2024-2025`)) * 10000
all_results_dot$incidence_proj <-
  (all_results_dot$projections / (all_results_dot$`U5_pop_2027-2028` + all_results_dot$`U5_pop_2028-2029`)) * 10000


plot_df <- all_results_dot %>%
  mutate(
    marker_shape = ifelse(Scenario == "cov_50_rela", 8, 16),
    Scenario_f = factor(
      Scenario,
      levels = c("cov_50_rela", "Cov_60", "Cov_40", "Cov_20"),
      labels = c("50% of Baseline Coverage", "60%", "40%", "20%")
    )
  )

coverage_df$joinkey <- tolower(coverage_df$region)
plot_df <- left_join(plot_df, coverage_df)

plot_df_filt <- plot_df %>%
  filter(
    Scenario_f %in% c("50% of Baseline Coverage", "60%", "40%", "20%")
  ) %>%
  mutate(state = fct_reorder(state, incidence_past))

line_df <- all_results_dot %>%
  group_by(state) %>%
  summarise(
    start = first(incidence_past),
    end = max(incidence_proj, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(state = factor(state, levels = levels(plot_df_filt$state)))

gradient_plot <- ggplot() +
  geom_segment(
    data = line_df,
    aes(x = start, xend = end, y = state, yend = state),
    color = "#D9D9D9FF", linewidth = 2.5
  ) +
  geom_point(
    data = plot_df_filt,
    aes(x = incidence_proj, y = state, color = Scenario_f,
        shape = factor(marker_shape)),
    size = 4, alpha = 0.8
  ) +
  geom_point(
    data = plot_df_filt,
    aes(x = incidence_past, y = state),
    shape = "|", size = 6, color = "black", stroke = 1.5
  ) +
  scale_shape_manual(values = c("16" = 16, "8" = 8), guide = "none") +
  scale_color_manual(
    values = c(
      "50% of Baseline Coverage" = "#9F2D55FF",
      "60%"                      = "#FFB242FF",
      "40%"                      = "#DE4F33FF",
      "20%"                      = "#341648FF"
    )
  ) +
  guides(color = guide_legend(
    override.aes = list(shape = c(8, 16, 16, 16))
  )) +
  theme_minimal() +
  labs(x = "Incidence (per 10,000)", y = NULL, color = "Coverage Scenario") +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.y = element_text(face = "bold", size = 11)
  )

fig2 <- p1 + gradient_plot + plot_layout(widths = c(3.5, 2))

ggsave(
  plot = fig2,
  filename = "./figure_renders/0309_state_level_comparison.png",
  device = agg_png, width = 16, height = 7, units = "in", dpi = 320
)
