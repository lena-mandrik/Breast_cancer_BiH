data_filtered <- data %>%
  filter(Year_diagnosis %in% c(2019, 2022)) %>%
  mutate(
    stage_group = case_when(
      Stage_diagnosis %in% c(1, 2) ~ "Early stage",
      Stage_diagnosis %in% c(3, 4) ~ "Late stage",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(stage_group))
stage_by_year <- data_filtered %>%
  group_by(Year_diagnosis, stage_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Year_diagnosis) %>%
  mutate(
    total = sum(n),
    proportion = n / total,
    proportion_percent = proportion * 100,
    ci_lower = pmax(0, proportion - 1.96 * sqrt(proportion * (1 - proportion) / total)),
    ci_upper = pmin(1, proportion + 1.96 * sqrt(proportion * (1 - proportion) / total)),
    ci_lower_percent = ci_lower * 100,
    ci_upper_percent = ci_upper * 100
  )

ggplot(stage_by_year, aes(x = stage_group, y = proportion_percent, fill = as.factor(Year_diagnosis))) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = ci_lower_percent, ymax = ci_upper_percent),
                width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = paste0(round(proportion_percent, 1), "%")),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(
    x = "Stage at Diagnosis",
    y = "Proportion (%)",
    fill = "Year of Diagnosis"
  ) +
  ylim(0, 100) +
  theme_minimal()

###############
#
library(dplyr)
library(ggplot2)

# 1. Filter and create year group variable
data_filtered <- data %>%
  filter(!is.na(Year_diagnosis)) %>%
  mutate(
    year_group = case_when(
      Year_diagnosis < 2020 ~ "<2020",
      Year_diagnosis >= 2021 ~ "≥2021",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(year_group))  # remove any remaining NAs

# 2. Create stage group
data_filtered <- data_filtered %>%
  mutate(
    stage_group = case_when(
      Stage_diagnosis %in% c(1, 2) ~ "Early stage",
      Stage_diagnosis %in% c(3, 4) ~ "Late stage",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(stage_group))

# 3. Calculate proportions and 95% CI within each year group
prop_summary <- data_filtered %>%
  group_by(year_group, stage_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(year_group) %>%
  mutate(
    total = sum(count),
    proportion = count / total,
    proportion_percent = proportion * 100,
    ci_lower = pmax(0, proportion - 1.96 * sqrt(proportion * (1 - proportion) / total)),
    ci_upper = pmin(1, proportion + 1.96 * sqrt(proportion * (1 - proportion) / total)),
    ci_lower_percent = ci_lower * 100,
    ci_upper_percent = ci_upper * 100
  )

write.csv(prop_summary, "stage_distribution_by_year.csv", row.names = FALSE)

# 4. Plot
ggplot(prop_summary, aes(x = stage_group, y = proportion_percent, fill = stage_group)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = ci_lower_percent, ymax = ci_upper_percent), width = 0.2) +
  geom_text(aes(label = paste0(round(proportion_percent, 1), "%")), vjust = -0.5) +
  facet_wrap(~year_group) +
  labs(
    x = "Stage at Diagnosis",
    y = "Proportion (%)",
    title = "Stage Distribution in <2020 vs ≥2022 Diagnoses"
  ) +
  ylim(0, 100) +
  theme_minimal() +
  theme(legend.position = "none")

##################################################
# Pathway to diagnosis

library(dplyr)
library(ggplot2)

data_pathway <- data %>%
  filter(!is.na(Year_diagnosis), !is.na(Screen_or_sympt)) %>%
  mutate(
    Diagnosis_Period = case_when(
      Year_diagnosis < 2020 ~ "Before 2020",
      Year_diagnosis > 2021 ~ "After 2021",
      TRUE ~ NA_character_
    ),
    Screen_or_sympt_clean = case_when(
      grepl("screen", Screen_or_sympt, ignore.case = TRUE) ~ "Screen-detected",
      tolower(Screen_or_sympt) == "symptomatic" ~ "Symptomatic",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Diagnosis_Period), !is.na(Screen_or_sympt_clean))


pathway_summary <- data_pathway %>%
  group_by(Diagnosis_Period, Screen_or_sympt_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Diagnosis_Period) %>%
  mutate(
    total = sum(count),
    proportion = count / total,
    proportion_percent = proportion * 100,
    ci_lower = pmax(0, proportion - 1.96 * sqrt(proportion * (1 - proportion) / total)),
    ci_upper = pmin(1, proportion + 1.96 * sqrt(proportion * (1 - proportion) / total)),
    ci_lower_percent = ci_lower * 100,
    ci_upper_percent = ci_upper * 100
  )


ggplot(pathway_summary, aes(x = Screen_or_sympt_clean, y = proportion_percent, fill = Screen_or_sympt_clean)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = ci_lower_percent, ymax = ci_upper_percent), width = 0.2) +
  geom_text(aes(label = paste0(round(proportion_percent, 1), "%")), vjust = -0.5) +
  facet_wrap(~Diagnosis_Period) +
  labs(
    x = "Pathway to Diagnosis",
    y = "Proportion (%)",
    title = "Diagnostic Pathway: Before 2020 vs After 2021"
  ) +
  ylim(0, 100) +
  theme_minimal() +
  theme(legend.position = "none")
##
