# Load libraries
library(ggplot2)
library(dplyr)
library(tibble)

# -----------------------
# Input data
data <- data.frame(
  interval = c(
    "Time from first symptoms to GP",                    # 1
    "Time to refer from GP to secondary care",           # 2
    "Time from GP referral to hospital admission",       # 3
    "Time from symptoms to diagnosis"                    # 4
  ),
  mean_days = c(38.1, 1.3, 37.3, 70),
  lower_CI = c(23.4, 0.59, 29.3, 58.9),
  upper_CI = c(52.8, 2.02, 45.3, 82.5),
  stringsAsFactors = FALSE
)

# Define names
i1 <- "Time from first symptoms to GP"
i2 <- "Time to refer from GP to secondary care"
i3 <- "Time from GP referral to hospital admission"
i4 <- "Time from symptoms to diagnosis"
i5 <- "System diagnostic intervals"  # derived = i2 + i3

# Extract scalar values
m1 <- data$mean_days[data$interval == i1]; l1 <- data$lower_CI[data$interval == i1]; u1 <- data$upper_CI[data$interval == i1]
m2 <- data$mean_days[data$interval == i2]; l2 <- data$lower_CI[data$interval == i2]; u2 <- data$upper_CI[data$interval == i2]
m3 <- data$mean_days[data$interval == i3]; l3 <- data$lower_CI[data$interval == i3]; u3 <- data$upper_CI[data$interval == i3]
m4 <- data$mean_days[data$interval == i4]; l4 <- data$lower_CI[data$interval == i4]; u4 <- data$upper_CI[data$interval == i4]

# -----------------------
# Compute start/end (mean and CI bounds)
# -----------------------

# Interval 1: starts at 0
start1_mean <- 0
start1_low  <- 0
start1_high <- 0
end1_mean   <- m1
end1_low    <- l1
end1_high   <- u1

# Interval 2: starts at end of 1
start2_mean <- end1_mean
start2_low  <- end1_low
start2_high <- end1_high
end2_mean   <- start2_mean + m2
end2_low    <- start2_low + l2
end2_high   <- start2_high + u2

# Interval 3: starts at end of 2
start3_mean <- end2_mean
start3_low  <- end2_low
start3_high <- end2_high
end3_mean   <- start3_mean + m3
end3_low    <- start3_low + l3
end3_high   <- start3_high + u3

# Interval 4: starts same as 1 (time 0)
start4_mean <- start1_mean
start4_low  <- start1_low
start4_high <- start1_high
end4_mean   <- start4_mean + m4
end4_low    <- start4_low + l4
end4_high   <- start4_high + u4

# Interval 5: starts same as 2 (end of 1), duration = 2 + 3
start5_mean <- start2_mean
start5_low  <- start2_low
start5_high <- start2_high
end5_mean   <- start5_mean + m2 + m3
end5_low    <- start5_low + l2 + l3
end5_high   <- start5_high + u2 + u3

# -----------------------
# Build plotting dataset
# -----------------------
plot_df <- tibble(
  interval = c(i1, i2, i3, i4, i5),
  start_mean = c(start1_mean, start2_mean, start3_mean, start4_mean, start5_mean),
  end_mean   = c(end1_mean,   end2_mean,   end3_mean,   end4_mean,   end5_mean),
  start_low  = c(start1_low,  start2_low,  start3_low,  start4_low,  start5_low),
  end_low    = c(end1_low,    end2_low,    end3_low,    end4_low,    end5_low),
  start_high = c(start1_high, start2_high, start3_high, start4_high, start5_high),
  end_high   = c(end1_high,   end2_high,   end3_high,   end4_high,   end5_high)
) %>%
  mutate(
    mean_length = end_mean - start_mean,
    mid_mean = (start_mean + end_mean) / 2,
    ci_xmin = start_low,
    ci_xmax = end_high
  )

# Order for plotting (top to bottom)
plot_df$interval <- factor(plot_df$interval,
                           levels = rev(c(i1, i2, i3, i4, i5)))

# -----------------------
# Plot
# -----------------------
ggplot(plot_df, aes(y = interval)) +
  # CI segment (light)
  geom_segment(aes(x = ci_xmin, xend = ci_xmax, yend = interval),
               size = 6, colour = "grey80", lineend = "round") +
  # Mean segment (blue)
  geom_segment(aes(x = start_mean, xend = end_mean, yend = interval),
               size = 4, colour = "#0072B2", lineend = "round") +
  # Start and end points
  geom_point(aes(x = start_mean), size = 2.2, colour = "#004C6D") +
  geom_point(aes(x = end_mean), size = 2.2, colour = "#004C6D") +
  # Label with duration
  geom_text(aes(x = mid_mean, label = paste0(round(mean_length, 1), " d")),
            vjust = -1.4, size = 3.6) +
  labs(
    title = "Diagnostic Pathway — Mean Intervals with 95% CI",
    x = "Time (days)",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 11)
  )
