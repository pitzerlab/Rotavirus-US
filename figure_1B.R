library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(waffle)

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
# Plot: US-level Projections (2026+): best fit vs comparison "2026-07-01" "2031-06-01"
# ============================================================

sim_cols <- names(combined_df)[startsWith(names(combined_df), "sim")]
cutoff_date <- as.Date("2025-12-31")
usa_df <- combined_df %>% filter(sheet_origin == "usa") %>% select(-cov_94_scaled)


#################
# 
#################
# 2025/26, 
# 2026/27, 2027/28, 2028/29, 2029/30, 2030/31

baseline_df <- usa_df %>%
  filter(Date >= as.Date("2026-07-01") & Date < as.Date("2031-07-01")) %>%
  select(Date, RVGE = `best_fit`)
hist_sum<- sum(baseline_df$RVGE) # 18143.12

# US-level waffle plot estimates 
proj_cols <- c("Cov_60", "Cov_40", "cov_50_rela","Cov_20")
target_order<- c("Cov_60", "Cov_40", "cov_50_rela","Cov_20")


proj_df <- usa_df %>%
  filter(Date >= as.Date("2026-07-01") & Date < as.Date("2031-07-01")) %>%
  select(Date, all_of(proj_cols)) %>%
  pivot_longer(cols = -Date, names_to = "Scenario", values_to = "RVGE") 

proj_df$Scenario <- factor(proj_df$Scenario, levels = target_order)

proj_df<- proj_df %>% group_by(Scenario) %>% summarise(proj_sum =  sum(RVGE))



# 1. Prepare the data
baseline_val <- hist_sum
df_scenarios<- data.frame(
  proj_df, 
  color_hex =c("#FFB242FF", "#DE4F33FF", "#9F2D55FF", "#341648FF"))
df_scenarios<- df_scenarios %>% 
  rename(total = proj_sum)
df_scenarios$Scenario <- factor(df_scenarios$Scenario, levels = target_order)


# 2. Reshape for Waffle (1 square = 1000 cases)
waffle_df <- df_scenarios %>%
  mutate(
    Baseline = round(baseline_val / 1000),
    Additional = round((total - baseline_val) / 1000)
  ) %>%
  pivot_longer(cols = c(Baseline, Additional), names_to = "Category", values_to = "n")%>%
  mutate(
    fold_change = paste0(round(total / baseline_val, 1), "x increase"),
    perc_inc = paste0("+", round(((total - baseline_val) / baseline_val) * 100), "% more cases")
  )


vax_labels <- c(
  "Cov_60"      = "Vaccine Coverage: 60%",
  "Cov_40"      = "Vaccine Coverage: 40%",
  "cov_50_rela" = "50% Reduction from Current Vaccine Coverage",
  "Cov_20"      = "Vaccine Coverage: 20%"
)

# 3. Plotting
waffle_plot <- ggplot(
  waffle_df,
  aes(fill = interaction(Category, Scenario), values = n)
) +
  geom_waffle(color = "white", size = 0.5, n_rows = 12, flip = FALSE) +
  facet_wrap(~Scenario, ncol = 4, labeller = labeller(Scenario = vax_labels)) +
  scale_fill_manual(
    values = c(
      "Baseline.Cov_60"        = "#D3D3D3",
      "Additional.Cov_60"      = "#FFB242FF",
      "Baseline.Cov_40"        = "#D3D3D3",
      "Additional.Cov_40"      = "#DE4F33FF",
      "Baseline.cov_50_rela"   = "#D3D3D3",
      "Additional.cov_50_rela" = "#9F2D55FF",
      "Baseline.Cov_20"        = "#D3D3D3",
      "Additional.Cov_20"      = "#341648FF"
    ),
    guide = "none"
  ) +
  coord_equal() +
  theme_void() +
  theme(
    strip.text = element_blank()
  )

ggsave(
  plot = waffle_plot,
  filename = "./figure_renders/0309_waffle_plot.jpg",
  device = "jpeg", width = 12, height = 7, dpi = 200
)
