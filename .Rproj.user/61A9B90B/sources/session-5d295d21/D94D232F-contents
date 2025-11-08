library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)

# --- Clean each treatment delay using your custom function ---

# Surgery delay
data$time_surgery_clean <- clean_time_diff(
  data,
  date_col1 = "Date_surgery_request",
  date_col2 = "Date_surgery",
  new_col_name = "time_surgery_clean",
  remove_negatives = TRUE,
  remove_outliers = TRUE
)

# Radiotherapy delay
data$time_radio_clean <- clean_time_diff(
  data,
  date_col1 = "Date_radio_request",
  date_col2 = "Date_1st_radio_cycle",
  new_col_name = "time_radio_clean",
  remove_negatives = TRUE,
  remove_outliers = TRUE
)

# Chemotherapy delay
data$time_chemo_clean <- clean_time_diff(
  data,
  date_col1 = "Date_chemo_request",
  date_col2 = "Date_1st_chemo_cycle",
  new_col_name = "time_chemo_clean",
  remove_negatives = TRUE,
  remove_outliers = TRUE
)

# --- Combine and reshape for plotting ---

delay_tr_clean <- data %>%
  select(N, Residency_group, Age_diagnosis,
         time_surgery_clean, time_radio_clean, time_chemo_clean) %>%
  pivot_longer(
    cols = starts_with("time_"),
    names_to = "delay_stage",
    values_to = "delay_days"
  ) %>%
  filter(!is.na(delay_days))

# --- Rename variables for clearer plot labels ---
delay_tr_clean$delay_stage <- recode(
  delay_tr_clean$delay_stage,
  time_surgery_clean = "Surgery",
  time_radio_clean = "Radiotherapy",
  time_chemo_clean = "Chemotherapy"
)

# --- Create boxplot of cleaned data ---
p <- ggplot(delay_tr_clean, aes(x = delay_stage, y = delay_days)) +
  geom_boxplot(outlier.shape = 19, fill = "#69b3a2", alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  labs(
    x = "Treatment Type",
    y = "Days"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# --- Print plot ---
print(p)

summary_stats <- delay_tr_clean %>%
  group_by(delay_stage) %>%
  summarise(
    N = sum(!is.na(delay_days)),
    Mean = mean(delay_days, na.rm = TRUE),
    Median = median(delay_days, na.rm = TRUE),
    SD = sd(delay_days, na.rm = TRUE),
    IQR = IQR(delay_days, na.rm = TRUE),
    CI_low = Mean - 1.96 * SD / sqrt(N),
    CI_high = Mean + 1.96 * SD / sqrt(N)
  )

print(summary_stats)
