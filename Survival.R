# Progression free and survival
library(dplyr)
library(survival)
library(survminer)

Survival <- data %>%
  mutate(
    Date_diagnosis = as.Date(Date_diagnosis),
    Date_death = as.Date(Date_death),
    Date_last_visit = as.Date(Date_last_visit),

    # Define event status: 1 = death, 0 = censored
    Status_dead = ifelse(!is.na(Date_death), 1, 0),

    # Define event or censoring date
    Date_event = ifelse(Status_dead == 1, Date_death, Date_last_visit),
    Date_event = as.Date(Date_event, origin = "1970-01-01"),  # Just in case

    # Time-to-event in days
    Time_days = as.numeric(Date_event - Date_diagnosis),

    # filter cohort (diagnosed 2019–2022)
    Year_dx = as.numeric(format(Date_diagnosis, "%Y"))
  ) %>%
  filter(Year_dx >= 2019 & Year_dx <= 2022)


km_fit <- survfit(Surv(Time_days, Status_dead) ~ 1, data = Survival)

# Print survival at 1 and 3 years
summary(km_fit, times = c(365, 1095))

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
  ## 1.  Ensure all date variables are true Date objects
  mutate(
    Date_diagnosis   = as.Date(Date_diagnosis),
    Date_last_visit  = as.Date(Date_last_visit),
    Date_death       = as.Date(Date_death)     # keep: needed for cancer deaths
  ) %>%

  ## 2.  Define the PFS event (1 = progressed / relapsed / metastatic OR died of cancer)
  mutate(
    Event_PFS = case_when(
      Last_status == "Relapse/progressed/metastatic"           ~ 1,
      Last_status == "Dead" & Reason_death == "Cancer"         ~ 1,
      Last_status == "Disease Free/Remission"                 ~ 0,
      TRUE                                                     ~ 0        # any other status = censored
    ),

    ## 3.  Date of event or censoring
    Date_event_PFS = case_when(
      Last_status == "Relapse/progressed/metastatic"           ~ Date_last_visit,         # progression date
      Last_status == "Dead" & Reason_death == "Cancer" &
        !is.na(Date_death)                                     ~ Date_death,              # cancer death
      TRUE                                                     ~ Date_last_visit          # censored
    ),

    ## 4.  Time from diagnosis to event/censor in days
    Time_PFS_days = as.numeric(Date_event_PFS - Date_diagnosis),

    ## 5.  Keep only the 2019-2022 incident cohort
    Year_dx = as.numeric(format(Date_diagnosis, "%Y"))
  ) %>%
  filter(Year_dx >= 2019 & Year_dx <= 2022)

km_PFS <- survfit(Surv(Time_PFS_days, Event_PFS) ~ 1, data = PFS)

## Survival at 1 year and 3 years
summary(km_PFS, times = c(365, 1095))

## PFS plot
ggsurvplot(
  km_PFS,
  conf.int  = TRUE,
  risk.table = TRUE,
  xlab = "Time since diagnosis (days)",
  ylab = "Progression-free survival probability",
  title = "Kaplan–Meier Curve for Progression-Free Survival"
)


