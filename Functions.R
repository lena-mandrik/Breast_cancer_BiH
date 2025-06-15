# remove outliers using IQR method

remove_iqr_outliers_by_group <- function(data, group_col, value_col) {
  data %>%
    group_by(across(all_of(group_col))) %>%
    filter({
      values <- .data[[value_col]]
      values <- values[values >= 0]  # Remove negative values first
      Q1 <- quantile(values, 0.25, na.rm = TRUE)
      Q3 <- quantile(values, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      .data[[value_col]] >= 0 &
        .data[[value_col]] >= (Q1 - 1.5 * IQR) &
        .data[[value_col]] <= (Q3 + 1.5 * IQR)
    }) %>%
    ungroup()
}


remove_outliers_by_group <- function(data, group_cols, value_col) {
  data %>%
    group_by(across(all_of(group_cols))) %>%
    filter({
      values <- .data[[value_col]]
      # Filter to keep only non-negative numeric values
      values_no_neg <- values[values >= 0]
      Q1 <- quantile(values_no_neg, 0.25, na.rm = TRUE)
      Q3 <- quantile(values_no_neg, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1

      # Keep rows where value is non-negative and within IQR bounds
      .data[[value_col]] >= 0 &
        .data[[value_col]] >= (Q1 - 1.5 * IQR) &
        .data[[value_col]] <= (Q3 + 1.5 * IQR)
    }) %>%
    ungroup()
}

remove_iqr_outliers <- function(data, value_col) {
  values <- data[[value_col]]

  # Remove negative or illogical values first
  values <- values[values >= 0]

  Q1 <- quantile(values, 0.25, na.rm = TRUE)
  Q3 <- quantile(values, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1

  # Define lower and upper bounds
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR

  # Filter rows in original data where values are within bounds and >= 0
  data %>%
    filter(
      .data[[value_col]] >= 0,
      .data[[value_col]] >= lower_bound,
      .data[[value_col]] <= upper_bound
    )
}

####################


# Define a function to remove negative and IQR outliers
clean_delay_data <- function(df, pathway_label) {
  df_clean <- df %>%
    filter(Pathway_refer == pathway_label) %>%
    mutate(
      time_sympt_to_diag = as.numeric(as.Date(Date_diagnosis) - as.Date(Date_1st_sympt)),
      time_sympt_to_ref = as.numeric(as.Date(Date_1st_refer) - as.Date(Date_1st_sympt)),
      time_ref_to_gp_ref = as.numeric(as.Date(Date_GP_ref_CCUoS) - as.Date(Date_1st_refer)),
      time_gp_ref_to_admit = as.numeric(as.Date(Date_1st_admission) - as.Date(Date_GP_ref_CCUoS))
    ) %>%
    select(N, Residency_group, time_sympt_to_diag, time_sympt_to_ref, time_ref_to_gp_ref, time_gp_ref_to_admit) %>%
    pivot_longer(
      cols = starts_with("time"),
      names_to = "delay_stage",
      values_to = "delay_days"
    ) %>%
    filter(!is.na(delay_days), delay_days >= 0)  # Remove NA and negative delays

  # Remove IQR outliers within each delay stage
  df_clean <- df_clean %>%
    group_by(delay_stage) %>%
    filter({
      Q1 <- quantile(delay_days, 0.25, na.rm = TRUE)
      Q3 <- quantile(delay_days, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      delay_days >= (Q1 - 1.5 * IQR) & delay_days <= (Q3 + 1.5 * IQR)
    }) %>%
    ungroup()

  # Record delay_stage
  df_clean$delay_stage <- recode(df_clean$delay_stage,
                                 time_sympt_to_diag = "Symptoms to Diagnosis",
                                 time_sympt_to_ref = "Symptoms to First Referral",
                                 time_ref_to_gp_ref = "Referral to GP Referral",
                                 time_gp_ref_to_admit = "GP Referral to Admission"
  )

  # Order factor levels
  df_clean$delay_stage <- factor(df_clean$delay_stage,
                                 levels = c("Symptoms to First Referral",
                                            "Referral to GP Referral",
                                            "GP Referral to Admission",
                                            "Symptoms to Diagnosis"))

  # Label pathway
  df_clean$Pathway_group <- pathway_label

  return(df_clean)
}

#########

# Function to calculate delays and apply IQR cleaning to each column
clean_time_delays <- function(df) {
  df <- df %>%
    mutate(
      Time_Symptom_to_Referral = as.numeric(difftime(Date_1st_refer, Date_1st_sympt, units = "days")),
      Time_Referral_to_Diagnosis = as.numeric(difftime(Date_diagnosis, Date_1st_refer, units = "days")),
      Time_Symptom_to_Diagnosis = as.numeric(difftime(Date_diagnosis, Date_1st_sympt, units = "days")),
      Time_Symptom_to_Treatment = as.numeric(difftime(Date_1st_chemo_cycle, Date_1st_sympt, units = "days"))
    ) %>%
    filter(
      Time_Symptom_to_Referral >= 0,
      Time_Referral_to_Diagnosis >= 0,
      Time_Symptom_to_Diagnosis >= 0,
      Time_Symptom_to_Treatment >= 0
    )

  delay_vars <- c("Time_Symptom_to_Referral", "Time_Referral_to_Diagnosis",
                  "Time_Symptom_to_Diagnosis", "Time_Symptom_to_Treatment")

  for (var in delay_vars) {
    q1 <- quantile(df[[var]], 0.25, na.rm = TRUE)
    q3 <- quantile(df[[var]], 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    df <- df %>% filter(df[[var]] >= lower & df[[var]] <= upper)
  }

  return(df %>% select(all_of(delay_vars)))
}


# Descriptive function

if (!requireNamespace("stats", quietly = TRUE)) install.packages("stats")

# calculate stat from beta distribution
f.CI.beta <- function(pos, total) {
  if (total == 0) return(rep(NA, 4))
  a <- pos + 1
  b <- total - pos + 1
  mean <- a / (a + b)
  sd <- sqrt((a * b) / ((a + b)^2 * (a + b + 1)))
  ci <- qbeta(c(0.025, 0.975), a, b)
  return(c(Proportion = mean, SD = sd, CI_lower = ci[1], CI_upper = ci[2]))
}

# Descriptive Statistics Function
get_stats <- function(variable, data){

  var_name <- deparse(substitute(variable))
  var_data <- data %>% pull({{ variable }})

  if (is.numeric(var_data)) {
    # Continuous variable
    n <- sum(!is.na(var_data))
    mean_val <- mean(var_data, na.rm = TRUE)
    sd_val <- sd(var_data, na.rm = TRUE)
    se <- sd_val / sqrt(n)
    ci_range <- qt(0.975, df = n - 1) * se

    stats <- tibble(
      mean_value = mean_val,
      median_value = median(var_data, na.rm = TRUE),
      sd_value = sd_val,
      n = n,
      ci_low = mean_val - ci_range,
      ci_high = mean_val + ci_range,
      iqr_value = IQR(var_data, na.rm = TRUE)
    )
  } else {
    # Categorical variable with 95% CI
    stats <- data %>%
      filter(!is.na({{ variable }})) %>%
      count({{ variable }}) %>%
      mutate(
        variable_name = var_name,
        level = as.character({{ variable }}),
        proportion = n / sum(n),
        se = sqrt(proportion * (1 - proportion) / sum(n)),
        ci_low = proportion - 1.96 * se,
        ci_high = proportion + 1.96 * se
      ) %>%
      mutate(across(c(proportion, ci_low, ci_high), ~ . * 100))  # Convert to percentage
  }

  return(stats)
}

### Plotting function
plot_categorical_var <- function(data, variable, xlab = NULL) {
  # Convert to symbol for use in tidy evaluation
  variable_sym <- rlang::ensym(variable)
  var_name <- rlang::as_name(variable_sym)

  freq_table <- data %>%
    filter(!is.na(!!variable_sym)) %>%
    count(!!variable_sym) %>%
    mutate(
      proportion = 100 * n / sum(n),
      ci = 1.96 * sqrt(proportion * (100 - proportion) / sum(n))
    )

  print(freq_table)

  # Calculate max y for ylim and label position
  ymax <- max(freq_table$proportion + freq_table$ci) + 10

  ggplot(freq_table, aes(x = !!variable_sym, y = proportion, fill = !!variable_sym)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = proportion - ci, ymax = proportion + ci), width = 0.2) +
    geom_text(aes(label = paste0(round(proportion, 1), "%"), y = proportion + ci + 2), size = 3.5) +
    labs(
      title = NULL,
      x = ifelse(is.null(xlab), var_name, xlab),
      y = "Percentage"
    ) +
    ylim(0, ymax) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 9, hjust = 1),
      legend.position = "none"
    )
}




# General function that should be applicable to dates to remove the outliers and illogical values
# and return the calculated time from two dates

clean_time_diff <- function(df,
                            date_col1,
                            date_col2,
                            new_col_name = "time_diff",
                            remove_negatives = TRUE,
                            remove_outliers = TRUE) {

  # Calculate raw time difference
  df[[new_col_name]] <- as.numeric(as.Date(df[[date_col2]]) - as.Date(df[[date_col1]]))

  # Remove negative values if requested
  if (remove_negatives) {
    df[[new_col_name]][df[[new_col_name]] < 0] <- NA
  }

  # Remove IQR outliers if requested
  if (remove_outliers) {
    qnt <- quantile(df[[new_col_name]], probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- 1.5 * IQR(df[[new_col_name]], na.rm = TRUE)
    df[[new_col_name]][df[[new_col_name]] < (qnt[1] - iqr) |
                         df[[new_col_name]] > (qnt[2] + iqr)] <- NA
  }

  return(df[[new_col_name]])
}


# function to calculate summary outputs on time to procedures, save and plot them
analyze_procedure_times <- function(time_data,
                                    plot_title = "Time to Diagnostic Procedures",
                                    y_lab = "Days",
                                    include_outliers = TRUE) {

  # Convert to data frame if it's a matrix
  if (is.matrix(time_data)) {
    time_data <- as.data.frame(time_data)
  }

  # 1. Calculate summary statistics
  summary_stats <- data.frame(
    Procedure = colnames(time_data),
    Mean = sapply(time_data, mean, na.rm = TRUE),
    Median = sapply(time_data, median, na.rm = TRUE),
    SD = sapply(time_data, sd, na.rm = TRUE),
    IQR = sapply(time_data, IQR, na.rm = TRUE),
    N = sapply(time_data, function(x) sum(!is.na(x))),
    row.names = NULL
  )

  # 2. Reshape data for plotting
  plot_data <- time_data %>%
    tibble::rownames_to_column("ID") %>%
    tidyr::pivot_longer(cols = -ID,
                        names_to = "Procedure",
                        values_to = "Days")

  # 3. Create boxplot
  p <- ggplot(plot_data, aes(x = Procedure, y = Days)) +
    geom_boxplot(outlier.shape = ifelse(include_outliers, 19, NA)) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
    labs(title = plot_title,
         y = y_lab) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # 4. Return results
  return(list(
    summary_table = summary_stats,
    boxplot = p
  ))
}
