# Progression free and survival
library(dplyr)
library(survival)
library(survminer)

Survival <- data %>%
  mutate(
    Date_diagnosis = as.Date(Date_diagnosis),
    Date_death = as.Date(Date_death),
    Date_last_visit = as.Date(Date_last_visit),

    Status_dead = ifelse(!is.na(Date_death), 1, 0),

    # Pick Date_death if present, otherwise Date_last_visit
    Date_event = coalesce(Date_death, Date_last_visit),

    Time_days = as.numeric(Date_event - Date_diagnosis),

    Year_dx = as.numeric(format(Date_diagnosis, "%Y"))
  ) %>%
  filter(Year_dx >= 2019 & Year_dx <= 2022)

km_fit <- survfit(Surv(Time_days, Status_dead) ~ 1, data = Survival)

# Print survival at 1 and 3 years
summary(km_fit, times = c(365, 1095, 1825))

ggsurvplot(
  km_fit,
  conf.int = TRUE,
  xlab = "Time since diagnosis (days)",
  ylab = "Survival probability",
  title = "Kaplan-Meier Survival Curve",
  risk.table = TRUE
)

##########################################################
###
# To calculate progression free survival

PFS <- data %>%
  mutate(
    Date_diagnosis = as.Date(Date_diagnosis),
    Date_last_visit = as.Date(Date_last_visit),
    Date_death = as.Date(Date_death),

    Event_PFS = case_when(
      Last_status == "Relapse/progressed/metastatic" ~ 1,
      Last_status == "Dead"                          ~ 1,
      TRUE                                           ~ 0
    ),

    Date_event_PFS = coalesce(
      if_else(Last_status == "Relapse/progressed/metastatic", Date_last_visit, NA_Date_),
      if_else(Last_status == "Dead" & !is.na(Date_death), Date_death, NA_Date_),
      Date_last_visit
    ),

    Time_PFS_days = as.numeric(Date_event_PFS - Date_diagnosis),

    Year_dx = as.numeric(format(Date_diagnosis, "%Y"))
  ) %>%
  filter(Year_dx >= 2019 & Year_dx <= 2022)

km_PFS <- survfit(Surv(Time_PFS_days, Event_PFS) ~ 1, data = PFS)

## Survival at 1 year and 3 years
summary(km_PFS, times = c(365, 1095, 1825))

## PFS plot
ggsurvplot(
  km_PFS,
  conf.int  = TRUE,
  risk.table = TRUE,
  xlab = "Time since diagnosis (days)",
  ylab = "Progression-free survival probability",
  title = "Kaplan–Meier Curve for Progression-Free Survival"
)


# --- COMBINE BOTH SURVIVAL OBJECTS ---
km_combined <- list(
  "Overall Survival" = km_fit,
  "Progression-Free Survival" = km_PFS
)

# --- PLOT BOTH CURVES ON THE SAME GRAPH ---
ggsurvplot_combine(
  fit = km_combined,
  conf.int = TRUE,
  xlab = "Time since diagnosis (days)",
  ylab = "Survival probability",
  legend.title = "Outcome",
  legend.labs = c("Overall Survival", "Progression-Free Survival"),
  risk.table = TRUE,
  palette = c("#0072B2", "#D55E00")  # blue & orange
)
