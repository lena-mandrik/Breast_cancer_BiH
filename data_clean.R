col_missing <- colSums(is.na(data))
missing_prop <- colSums(is.na(data)) / nrow(data)

# Create a dataframe of missing proportions
missing_df <- data.frame(
  column = names(missing_prop),
  proportion_missing = missing_prop
)

# How many variables?
n_vars <- nrow(missing_df)

# Define group size (split roughly equally into 4 parts)
group_size <- ceiling(n_vars / 4)

# Add a group variable
missing_df$group <- rep(1:4, each = group_size)[1:n_vars]

# Check the split
table(missing_df$group)


# Plot for each group
for (g in 1:4) {
  p <- ggplot(missing_df[missing_df$group == g, ],
              aes(x = reorder(column, -proportion_missing), y = proportion_missing)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = paste("Proportion of Missing Values (Group", g, ")"),
         x = "Column",
         y = "Proportion Missing") +
    theme_minimal()

  print(p)
}

#########################################################################
## Check formatting
#######################################################################
## Convert dates to the date format


# List of date-related columns to check
date_columns <- c(
  "Date_1st_sympt", "Date_1st_refer", "Date_GP_ref_CCUoS", "Date_1st_admission",
  "MM1st_date_req", "MM1st_date_cond", "US1st_date_req", "US1st_date_cond",
  "MRI1st_date_req", "MRI1st_date_cond", "Biopsy1st_date_req", "Biopsy1st_date_cond",
  "Biopsy_date_result", "Xray1st_date_req", "Xray1st_date_cond",
  "CT1st_date_req", "CT1st_date_cond", "Date_diagnosis", "Date_surgery_request",
  "Date_surgery", "Date_chemo_request", "Date_1st_chemo_cycle", "Date_last_chemo_cycle",
  "Date_radio_request", "Date_1st_radio_cycle", "Date_last_radio_cycle",
  "Date_hormon_prescr", "Date_target_prescr", "Date_last_visit", "Date_visit_b_last",
  "Date_death"
)

# Check the format of each date-related column
data %>%
  select(all_of(date_columns)) %>%
  summarise(across(everything(), ~ sum(is.na(ymd(.x, quiet = TRUE)))), .names = "Missing_{.col}") %>%
  bind_rows(data %>%
              select(all_of(date_columns)) %>%
              summarise(across(everything(), ~ sum(!is.na(ymd(.x, quiet = TRUE)))), .names = "Valid_{.col}"))


######################################################################
######################################################################
# Descriptive analysis of demographics
#colnames(data)

# 1. Calculate Age at Symptoms (Year of Birth - Date of First Symptoms)
data <- data %>%
  mutate(
    Year_1st_sympt = year(Date_1st_sympt),
    Age_at_sympt = ifelse(!is.na(Year_1st_sympt) & !is.na(Year_birth), Year_1st_sympt - Year_birth, NA)
  )

# 2. Calculate Age at Diagnosis (Year of Birth - Date of Diagnosis)
data <- data %>%
  mutate(
    Year_diagnosis = year(Date_diagnosis),
    Age_at_diagnosis = ifelse(!is.na(Year_diagnosis) & !is.na(Year_birth), Year_diagnosis - Year_birth, NA)
  )

# Calculate Age at symptoms and Age at diagnosis
data <- data %>%
  mutate(
    Age_symptoms = as.numeric(format(Date_1st_sympt, "%Y")) - Year_birth,
    Age_diagnosis = as.numeric(format(Date_diagnosis, "%Y")) - Year_birth,
    Canton_grouped = ifelse(Canton == "Sarajevo", "Sarajevo", "Other")
  )

# Residency
(residency_freq <- data %>%
  count(Residency) %>%
  mutate(percentage = 100 * n / sum(n)))

# Canton_grouped
(canton_freq <- data %>%
  count(Canton_grouped) %>%
  mutate(percentage = 100 * n / sum(n)))

residency_freq <- residency_freq %>% filter(!is.na(Residency))
canton_freq <- canton_freq %>% filter(!is.na(Canton_grouped))

### Update descriptives if other values with similar meaning were used

# For symptomatic vs screen pathway
data <- data %>%
  mutate(
    Screen_or_sympt = case_when(
      Screen_or_sympt %in% c("Screen: Self examination", "Screen: self-examination", "Screen: Self-examination", "Screen: self examination", "Symptomatic") ~ "Symptomatic",
      Screen_or_sympt %in% c("Screen: Mammography", "Screen: mammography") ~ "Screen mammography",
      TRUE ~ Screen_or_sympt
    )
  )

# For primary vs secondary pathway
data <- data %>%
  mutate(
    Pathway_refer = case_when(
      Pathway_refer %in% c("Primary", "Primary care (public)") ~ "Primary",
      Pathway_refer %in% c("Private", "private") ~ "Private",
      Pathway_refer %in% c("Secondary or tertiary care", "Secondary or tertiary care (public)") ~ "Secondary/tertiary",
      TRUE ~ Pathway_refer
    )
  )

# For stage at diagnosis
# For symptomatic vs screen pathway
data <- data %>%
  mutate(
    Stage_diagnosis = case_when(
      Stage_diagnosis %in% c("CIS", "DCIS") ~ "DCIS",
      TRUE ~ Stage_diagnosis
    )
  )


# 4. Get Descriptive Stats for Variables
# For continuous
(age_sympt_stats <- get_stats(Age_at_sympt, data))
(age_diag_stats <- get_stats(Age_at_diagnosis, data))
(year_diag <- get_stats(Year_diagnosis, data))

# For categorical
(canton <- get_stats(Canton_grouped, data))
(residence <- get_stats(Residency, data))
(Screen_or_sympt <- get_stats(Screen_or_sympt, data))
(Pathway_refer <- get_stats(Pathway_refer, data))
(Stage_diag <- get_stats(Stage_diagnosis, data))

continuous_vars <- c("Year_birth", "Age_at_sympt", "Age_at_diagnosis")
categorical_vars <- c("Residency", "Canton_grouped", "Screen_or_sympt", "Pathway_refer", "Stage_diagnosis")

write.csv(Stage_diag, "Stage_diag.csv", row.names=F)


# Continuous
continuous_summary <- purrr::map_dfr(continuous_vars, function(var) {
  get_stats(sym(var), data) %>%
    mutate(variable = var)
})
# Categorical
categorical_summary <- purrr::map_dfr(categorical_vars, function(var) {
  get_stats(!!rlang::sym(var), data) %>%
    dplyr::mutate(variable = var)
})


(summary_table <- bind_rows(continuous_summary, categorical_summary))

# View
print(summary_table)
write.csv(summary_table, "summary_table.csv", row.names = FALSE)

# Plot Age at Symptoms with CI (Only for available data)
# First calculate mean, median, and CI:
age_sympt_stats <- data %>%
  filter(!is.na(Age_at_sympt)) %>%
  summarise(
    mean_value = mean(Age_at_sympt, na.rm = TRUE),
    median_value = median(Age_at_sympt, na.rm = TRUE),
    ci_low = mean(Age_at_sympt, na.rm = TRUE) - qt(0.975, df = n() - 1) * sd(Age_at_sympt, na.rm = TRUE) / sqrt(n()),
    ci_high = mean(Age_at_sympt, na.rm = TRUE) + qt(0.975, df = n() - 1) * sd(Age_at_sympt, na.rm = TRUE) / sqrt(n())
  )

# Then plot:
# First calculate stats
age_sympt_stats <- data %>%
  filter(!is.na(Age_at_sympt)) %>%
  summarise(
    mean_value = mean(Age_at_sympt, na.rm = TRUE),
    median_value = median(Age_at_sympt, na.rm = TRUE),
    ci_low = mean(Age_at_sympt, na.rm = TRUE) - qt(0.975, df = n() - 1) * sd(Age_at_sympt, na.rm = TRUE) / sqrt(n()),
    ci_high = mean(Age_at_sympt, na.rm = TRUE) + qt(0.975, df = n() - 1) * sd(Age_at_sympt, na.rm = TRUE) / sqrt(n())
  )

# Create the plot
(ggplot(data %>% filter(!is.na(Age_at_sympt)), aes(x = Age_at_sympt)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = age_sympt_stats$mean_value), color = "black", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = age_sympt_stats$median_value), color = "black", linetype = "dotted", size = 1) +

  # Add Mean + CI label (small font, left side)
  annotate("text",
           x = min(data$Age_at_sympt, na.rm = TRUE) + 2,  # small shift right from y-axis
           y = Inf,
           label = paste0(
             "Mean: ", round(age_sympt_stats$mean_value, 1), "\n",
             "CI: [", round(age_sympt_stats$ci_low, 1), ", ", round(age_sympt_stats$ci_high, 1), "]"
           ),
           hjust = 0, vjust = 2, color = "black", size = 3) +

  # Add Median label
  annotate("text",
           x = min(data$Age_at_sympt, na.rm = TRUE) + 2,
           y = Inf,
           label = paste0(
             "Median: ", round(age_sympt_stats$median_value, 1)
           ),
           hjust = 0, vjust = 8, color = "black", size = 3) +

  theme_minimal() +
  labs(
    title = "Distribution of Age at Time of First Symptoms Presentation",
    x = "Age at Symptoms",
    y = "Frequency"
  ))

# Plot Age at Diagnosis with CI
(age_diag_stats <- data %>%
  filter(!is.na(Age_at_diagnosis)) %>%
  summarise(
    mean_value = mean(Age_at_diagnosis, na.rm = TRUE),
    median_value = median(Age_at_diagnosis, na.rm = TRUE),
    ci_low = mean(Age_at_diagnosis, na.rm = TRUE) - qt(0.975, df = n() - 1) * sd(Age_at_diagnosis, na.rm = TRUE) / sqrt(n()),
    ci_high = mean(Age_at_diagnosis, na.rm = TRUE) + qt(0.975, df = n() - 1) * sd(Age_at_diagnosis, na.rm = TRUE) / sqrt(n())
  ))

# Create the plot
(ggplot(data %>% filter(!is.na(Age_at_diagnosis)), aes(x = Age_at_diagnosis)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = age_diag_stats$mean_value), color = "black", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = age_diag_stats$median_value), color = "black", linetype = "dotted", size = 1) +

  # Add Mean + CI label (small font, left side)
  annotate("text",
           x = min(data$Age_at_diagnosis, na.rm = TRUE) + 2,  # small shift right from y-axis
           y = Inf,
           label = paste0(
             "Mean: ", round(age_diag_stats$mean_value, 1), "\n",
             "CI: [", round(age_diag_stats$ci_low, 1), ", ", round(age_diag_stats$ci_high, 1), "]"
           ),
           hjust = 0, vjust = 1, color = "black", size = 3) +

  # Add Median label
  annotate("text",
           x = min(data$Age_at_diagnosis, na.rm = TRUE) + 2,
           y = Inf,
           label = paste0(
             "Median: ", round(age_diag_stats$median_value, 1)
           ),
           hjust = 0, vjust = 5, color = "black", size = 3) +

  theme_minimal() +
  labs(
    title = "Distribution of Age at Diagnosis",
    x = "Age at Diagnosis",
    y = "Frequency"
  ))


######################################################
# Comparative statistics by Residency for Age at Symptoms

# compare age at diagnosis and treatment between Urban/city and Rural/Town
age_sympt_residency_stats <- data %>%
  filter(!is.na(Age_at_sympt)) %>%
  group_by(Residency) %>%
  summarise(
    mean_age_sympt = mean(Age_at_sympt, na.rm = TRUE),
    ci_low_age_sympt = mean(Age_at_sympt, na.rm = TRUE) - 1.96 * sd(Age_at_sympt, na.rm = TRUE) / sqrt(n()),
    ci_high_age_sympt = mean(Age_at_sympt, na.rm = TRUE) + 1.96 * sd(Age_at_sympt, na.rm = TRUE) / sqrt(n())
  )

# Create a group variable (residency_group)
data <- data %>%
  mutate(Residency_group = ifelse(Residency %in% c("Rural", "Town"), "Rural/Town", "Urban/City"))

#calculate summary statistics

# Age at symptom onset
age_sympt_stats <- data %>%
  filter(!is.na(Age_at_sympt)) %>%
  group_by(Residency_group) %>%
  summarise(
    mean = mean(Age_at_sympt, na.rm = TRUE),
    ci_low = mean - 1.96 * sd(Age_at_sympt, na.rm = TRUE) / sqrt(n()),
    ci_high = mean + 1.96 * sd(Age_at_sympt, na.rm = TRUE) / sqrt(n()),
    Age_type = "Symptom Onset"
  )

# Age at diagnosis
age_diag_stats <- data %>%
  filter(!is.na(Age_at_diagnosis)) %>%
  group_by(Residency_group) %>%
  summarise(
    mean = mean(Age_at_diagnosis, na.rm = TRUE),
    ci_low = mean - 1.96 * sd(Age_at_diagnosis, na.rm = TRUE) / sqrt(n()),
    ci_high = mean + 1.96 * sd(Age_at_diagnosis, na.rm = TRUE) / sqrt(n()),
    Age_type = "Diagnosis"
  )

# Combine
(summary_data <- bind_rows(age_sympt_stats, age_diag_stats))

# Compare two groups with the T test

t_test_sympt <- t.test(Age_at_sympt ~ Residency_group, data = data)
(t_test_sympt)
t_test_diag <- t.test(Age_at_diagnosis ~ Residency_group, data = data)
(t_test_diag)

# Plot outcomes

(ggplot(summary_data, aes(x = Age_type, y = mean, fill = Residency_group)) +
  geom_bar(stat = "identity", position = position_dodge(0.6), width = 0.5) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    position = position_dodge(0.6), width = 0.2
  ) +
  labs(
    title = "Mean Age at Symptom Onset and Diagnosis by Residency Group",
    x = "Age Type",
    y = "Mean Age (years)",
    fill = "Residency Group"
  ) +
  theme_minimal())

# Plot all categorical variable
categorical_vars <- c("Residency", "Canton_grouped", "Screen_or_sympt", "Pathway_refer", "Stage_diagnosis")


plots <- lapply(categorical_vars, function(var) {
  plot_categorical_var(data, !!rlang::sym(var))
})

print(plots[[1]])
print(plots[[2]])
print(plots[[3]])
print(plots[[4]])
print(plots[[5]])

